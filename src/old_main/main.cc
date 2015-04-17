/*
 *  Copyright (C) 2003-2012  Anders Gavare.  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright  
 *     notice, this list of conditions and the following disclaimer in the 
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE   
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 *
 *
 *  GXemul's main entry point.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "ComponentFactory.h"
#include "console.h"
#include "cpu.h"
#include "debugger.h"
#include "device.h"
#include "diskimage.h"
#include "emul.h"
#include "GXemul.h"
#include "machine.h"
#include "misc.h"
#include "settings.h"
#include "timer.h"
#include "UnitTest.h"


extern int single_step;
extern int force_debugger_at_exit;

extern int optind;
extern char *optarg;

struct settings *global_settings;

int extra_argc;
char **extra_argv;
char *progname;

size_t dyntrans_cache_size = DEFAULT_DYNTRANS_CACHE_SIZE;
static int skip_srandom_call = 0;


/*****************************************************************************
 *
 *  NOTE:  debug(), fatal(), and debug_indentation() are not re-entrant.
 *         The global variable quiet_mode can be used to suppress the output
 *         of debug(), but not the output of fatal().
 *
 *****************************************************************************/

int verbose = 0;
int quiet_mode = 0;

static int debug_indent = 0;
static int debug_currently_at_start_of_line = 1;


/*
 *  va_debug():
 *
 *  Used internally by debug() and fatal().
 */
static void va_debug(va_list argp, const char *fmt)
{
	char buf[DEBUG_BUFSIZE + 1];
	char *s;
	int i;

	buf[0] = buf[DEBUG_BUFSIZE] = 0;
	vsnprintf(buf, DEBUG_BUFSIZE, fmt, argp);

	s = buf;
	while (*s) {
		if (debug_currently_at_start_of_line) {
			for (i=0; i<debug_indent; i++)
				printf(" ");
		}

		printf("%c", *s);

		debug_currently_at_start_of_line = 0;
		if (*s == '\n' || *s == '\r')
			debug_currently_at_start_of_line = 1;
		s++;
	}
}


/*
 *  debug_indentation():
 *
 *  Modify the debug indentation.
 */
void debug_indentation(int diff)
{
	debug_indent += diff;
	if (debug_indent < 0)
		fprintf(stderr, "WARNING: debug_indent less than 0!\n");
}


/*
 *  debug():
 *
 *  Debug output (ignored if quiet_mode is set).
 */
void debug(const char *fmt, ...)
{
	va_list argp;

	if (quiet_mode)
		return;

	va_start(argp, fmt);
	va_debug(argp, fmt);
	va_end(argp);
}


/*
 *  fatal():
 *
 *  Fatal works like debug(), but doesn't care about the quiet_mode
 *  setting.
 */
void fatal(const char *fmt, ...)
{
	va_list argp;

	va_start(argp, fmt);
	va_debug(argp, fmt);
	va_end(argp);
}


/*****************************************************************************/


/*
 *  internal_w():
 *
 *  For internal use by gxemul itself.
 */
void internal_w(char *arg)
{
	if (arg == NULL || strncmp(arg, "W@", 2) != 0) {
		fprintf(stderr, "-W is for internal use by gxemul,"
		    " not for manual use.\n");
		exit(1);
	}

	arg += 2;

	switch (arg[0]) {
	case 'C':
		GXemul::GenerateHTMLListOfComponents(false);
		exit(0);
		break;
	case 'D':
		GXemul::DumpMachineAsHTML(arg + 1);
		break;
	case 'S':
		console_slave(arg + 1);
		break;
	case 'M':
		GXemul::GenerateHTMLListOfComponents(true);
		exit(0);
		break;
	case 'U':
		{
			int result = UnitTest::RunTests();

			// Hack to prevent leaks:
			ComponentFactory::UnregisterAllComponentClasses();

#ifndef NDEBUG
			int leaks = check_leaks();
			if (leaks > 0) {
				cerr << "Having memory leaks counts as failure to run the tests!\n";
				exit(1);
			}
#endif

			exit(result);
		}
		break;
	default:
		fprintf(stderr, "internal_w(): UNIMPLEMENTED arg = '%s'\n",
		    arg);
	}
}


/*****************************************************************************/


/*
 *  usage():
 *
 *  Prints program usage to stdout.
 */
static void usage(int longusage)
{

	printf("GXemul "VERSION"    "COPYRIGHT_MSG"\n"SECONDARY_MSG);
	printf("Read the source code and/or documentation for "
	    "other Copyright messages.\n");

	printf("\nUsage: %s [options] -e name [additional components and files [...]]\n", progname);
	printf("   or  %s [options] configfile\n", progname);
	printf("   or  %s -H\n", progname);
	printf("   or  %s -V\n", progname);

	if (longusage) {
		printf("\nOptions:\n");
		printf("  -B           Enable snapshotting (reverse stepping support).\n");
		printf("  -H           Display a list of available machine templates.\n");
		printf("  -e name      Start with a machine based on template 'name'.\n");
		printf("  -q           Quiet mode (suppress debug messages).\n");
		printf("  -V           Start up in interactive mode, paused.\n");
		printf("\n");
	}

	if (!longusage) {
		printf("\nLegacy usage: %s [machine, other, and general options] [file "
		    "[...]]\n", progname);
		printf("          or  %s [general options] @configfile\n", progname);

		printf("\nRun  %s -h  for help on command line options.\n",
		    progname);
		return;
	}

	printf("\n--------------------- The following are LEGACY options: ---------------------\n");

	printf("\nLegacy usage: %s [machine, other, and general options] [file "
	    "[...]]\n", progname);
	printf("          or  %s [general options] @configfile\n", progname);

	printf("\nMachine selection options:\n");
	printf("  -E t      try to emulate machine type t. (Use -H to get "
	    "a list of types.)\n");
	printf("  -e st     try to emulate machine subtype st. (Use this "
	    "with -E.)\n");

	printf("\nOther options:\n");
	printf("  -C x      try to emulate a specific CPU. (Use -H to get a "
	    "list of types.)\n");
	printf("  -d fname  add fname as a disk image. You can add \"xxx:\""
	    " as a prefix\n");
	printf("            where xxx is one or more of the following:\n");
	printf("                b      specifies that this is the boot"
	    " device\n");
	printf("                c      CD-ROM\n");
	printf("                d      DISK\n");
	printf("                f      FLOPPY\n");
	printf("                gH;S;  set geometry to H heads and S"
	    " sectors-per-track\n");
	printf("                i      IDE\n");
	printf("                oOFS;  set base offset to OFS (for ISO9660"
	    " filesystems)\n");
	printf("                r      read-only (don't allow changes to the"
	    " file)\n");
	printf("                s      SCSI\n");
	printf("                t      tape\n");
	printf("                V      add an overlay\n");
	printf("                0-7    force a specific ID\n");
	printf("  -I hz     set the main cpu frequency to hz (not used by "
	    "all combinations\n            of machines and guest OSes)\n");
	printf("  -i        display each instruction as it is executed\n");
	printf("  -J        disable dyntrans instruction combinations\n");
	printf("  -j name   set the name of the kernel; for DECstation "
	    "emulation, this passes\n            the name to the bootloader,"
	    " for example:\n");
	printf("                -j netbsd     (NetBSD/pmax)      "
	    "-j bsd      (OpenBSD/pmax)\n");
	printf("                -j vmsprite   (Sprite/pmax)      "
	    "-j vmunix   (Ultrix/RISC)\n");
	printf("            For other emulation modes, if the boot disk is an"
	    " ISO9660\n            filesystem, -j sets the name of the"
	    " kernel to load.\n");
	printf("  -M m      emulate m MBs of physical RAM\n");
	printf("  -N        display nr of instructions/second average, at"
	    " regular intervals\n");
	printf("  -n nr     set nr of CPUs (for SMP experiments)\n");
	printf("  -O        force netboot (tftp instead of disk), even when"
	    " a disk image is\n"
	    "            present (for DECstation, SGI, and ARC emulation)\n");
	printf("  -o arg    set the boot argument, for DEC, ARC, or SGI"
	    " emulation\n");
	printf("            (default arg for DEC is -a, for ARC/SGI -aN)\n");
	printf("  -p pc     add a breakpoint (remember to use the '0x' "
	    "prefix for hex!)\n");
	printf("  -Q        no built-in PROM emulation  (use this for "
	    "running ROM images)\n");
	printf("  -R        use random bootstrap cpu, instead of nr 0\n");
	printf("  -r        register dumps before every instruction\n");
	printf("  -S        initialize emulated RAM to random bytes, "
	    "instead of zeroes\n");
	printf("  -s f:name write statistics to file 'name', "
	    "f is one or more of the following:\n");
	printf("                v    virtual program counter\n");
	printf("                p    physical equivalent of program counter\n");
	printf("                i    internal ic->f representation of "
	    "the program counter\n");
	printf("            and optionally:\n");
	printf("                d    disable statistics gathering at "
	    "startup\n");
	printf("                o    overwrite instead of append\n");
	printf("  -T        halt on non-existant memory accesses\n");
	printf("  -t        show function trace tree\n");
	printf("  -U        enable slow_serial_interrupts_hack_for_linux\n");
#ifdef WITH_X11
	printf("  -X        use X11\n");
	printf("  -x        open up new xterms for emulated serial ports "
	    "(default is on when\n            using configuration files or"
	    " when X11 is used, off otherwise)\n");
	printf("  -Y n      scale down framebuffer windows by n x n times\n");
#endif /*  WITH_X11  */
	printf("  -Z n      set nr of graphics cards, for emulating a "
	    "dual-head or tripple-head\n"
	    "            environment (only for DECstation emulation)\n");
	printf("  -z disp   add disp as an X11 display to use for "
	    "framebuffers\n");

	printf("\nGeneral options:\n");
	printf("  -c cmd    add cmd as a command to run before starting "
	    "the simulation\n");
	printf("  -D        skip the srandom call at startup\n");
	printf("  -H        display a list of possible CPU and "
	    "machine types\n");
	printf("  -h        display this help message\n");
	printf("  -k n      set dyntrans translation caches to n MB (default"
	    " size is %i MB)\n", DEFAULT_DYNTRANS_CACHE_SIZE / 1048576);
	printf("  -K        force the debugger to be entered at the end "
	    "of a simulation\n");
	printf("  -q        quiet mode (don't print startup messages)\n");
	printf("  -V        start up in the single-step debugger, paused\n");
	printf("  -v        increase debug message verbosity\n");
	printf("\n");
	printf("If you are selecting a machine type to emulate directly "
	    "on the command line,\nthen you must specify one or more names"
	    " of files that you wish to load into\n"
	    "memory. Supported formats are:   ELF a.out ecoff srec syms raw\n"
	    "where syms is the text produced by running 'nm' (or 'nm -S') "
	    "on a binary.\n"
	    "To load a raw binary into memory, add \"address:\" in front "
	    "of the filename,\n"
	    "or \"address:skiplen:\" or \"address:skiplen:initialpc:\".\n"
	    "\nExamples:\n"
	    "    0xbfc00000:rom.bin                    for a raw ROM image\n"
	    "    0xbfc00000:0x100:rom.bin              for an image with "
	    "0x100 bytes header\n"
	    "    0xbfc00000:0x100:0xbfc00884:rom.bin   "
	    "start with pc=0xbfc00884\n\n");
}


/*
 *  get_cmd_args():
 *
 *  Reads command line arguments.
 */
int get_cmd_args(int argc, char *argv[], struct emul *emul,
	char ***diskimagesp, int *n_diskimagesp)
{
	int ch, res, using_switch_d = 0, using_switch_Z = 0;
	int using_switch_e = 0, using_switch_E = 0;
	bool using_switch_B = false;
	char *type = NULL, *subtype = NULL;
	int n_cpus_set = 0;
	int msopts = 0;		/*  Machine-specific options used  */
	struct machine *m = emul_add_machine(emul, NULL);

	const char *opts =
	    "BC:c:Dd:E:e:HhI:iJj:k:KM:Nn:Oo:p:QqRrSs:TtUVvW:"
#ifdef WITH_X11
	    "XxY:"
#endif
	    "Z:z:";

	while ((ch = getopt(argc, argv, opts)) != -1) {
		switch (ch) {
		case 'B':
			using_switch_B = true;
			break;
		case 'C':
			CHECK_ALLOCATION(m->cpu_name = strdup(optarg));
			msopts = 1;
			break;
		case 'c':
			emul->n_debugger_cmds ++;
			CHECK_ALLOCATION(emul->debugger_cmds = (char **)
			    realloc(emul->debugger_cmds,
			    emul->n_debugger_cmds * sizeof(char *)));
			CHECK_ALLOCATION(emul->debugger_cmds[emul->
			    n_debugger_cmds-1] = strdup(optarg));
			break;
		case 'D':
			skip_srandom_call = 1;
			break;
		case 'd':
			/*  diskimage_add() is called further down  */
			(*n_diskimagesp) ++;
			CHECK_ALLOCATION( (*diskimagesp) = (char **)
			    realloc(*diskimagesp,
			    sizeof(char *) * (*n_diskimagesp)) );
			CHECK_ALLOCATION( (*diskimagesp)[(*n_diskimagesp) - 1] =
			    strdup(optarg) );
			using_switch_d = 1;
			msopts = 1;
			break;
		case 'E':
			if (using_switch_E ++ > 0) {
				fprintf(stderr, "-E already used.\n");
				exit(1);
			}
			type = optarg;
			msopts = 1;
			break;
		case 'e':
			if (using_switch_e ++ > 0) {
				fprintf(stderr, "-e already used.\n");
				exit(1);
			}
			subtype = optarg;
			msopts = 1;
			break;
		case 'H':
			GXemul::ListTemplates();
			printf("--------------------------------------------------------------------------\n\n");
			printf("The following applies to the LEGACY modes only:\n\n");
			machine_list_available_types_and_cpus();
			exit(1);
		case 'h':
			usage(1);
			exit(1);
		case 'I':
			m->emulated_hz = atoi(optarg);
			msopts = 1;
			break;
		case 'i':
			m->instruction_trace = 1;
			msopts = 1;
			break;
		case 'J':
			m->allow_instruction_combinations = 0;
			msopts = 1;
			break;
		case 'j':
			CHECK_ALLOCATION(m->boot_kernel_filename =
			    strdup(optarg));
			msopts = 1;
			break;
		case 'k':
			dyntrans_cache_size = atoi(optarg) * 1048576;
			if (dyntrans_cache_size < 1) {
				fprintf(stderr, "The dyntrans cache size must"
				    " be at least 1 MB.\n");
				exit(1);
			}
			break;
		case 'K':
			force_debugger_at_exit = 1;
			break;
		case 'M':
			m->physical_ram_in_mb = atoi(optarg);
			msopts = 1;
			break;
		case 'N':
			m->show_nr_of_instructions = 1;
			msopts = 1;
			break;
		case 'n':
			m->ncpus = atoi(optarg);
			n_cpus_set = 1;
			msopts = 1;
			break;
		case 'O':
			m->force_netboot = 1;
			msopts = 1;
			break;
		case 'o':
			CHECK_ALLOCATION(m->boot_string_argument =
			    strdup(optarg));
			msopts = 1;
			break;
		case 'p':
			machine_add_breakpoint_string(m, optarg);
			msopts = 1;
			break;
		case 'Q':
			m->prom_emulation = 0;
			msopts = 1;
			break;
		case 'q':
			quiet_mode = 1;
			break;
		case 'R':
			m->use_random_bootstrap_cpu = 1;
			msopts = 1;
			break;
		case 'r':
			m->register_dump = 1;
			msopts = 1;
			break;
		case 'S':
			m->random_mem_contents = 1;
			msopts = 1;
			break;
		case 's':
			machine_statistics_init(m, optarg);
			msopts = 1;
			break;
		case 'T':
			m->halt_on_nonexistant_memaccess = 1;
			msopts = 1;
			break;
		case 't':
			m->show_trace_tree = 1;
			msopts = 1;
			break;
		case 'U':
			m->slow_serial_interrupts_hack_for_linux = 1;
			msopts = 1;
			break;
		case 'V':
			single_step = ENTER_SINGLE_STEPPING;
			break;
		case 'v':
			verbose ++;
			break;
		case 'W':
			internal_w(optarg);
			exit(0);
		case 'X':
			m->x11_md.in_use = 1;
			msopts = 1;
			/*  FALL-THROUGH  */
		case 'x':
			console_allow_slaves(1);
			break;
		case 'Y':
			m->x11_md.scaledown = atoi(optarg);
			if (m->x11_md.scaledown < -1) {
				m->x11_md.scaleup = - m->x11_md.scaledown;
				m->x11_md.scaledown = 1;
			}
			if (m->x11_md.scaledown < 1) {
				fprintf(stderr, "Invalid scaledown value.\n");
				exit(1);
			}
			msopts = 1;
			break;
		case 'Z':
			m->n_gfx_cards = atoi(optarg);
			using_switch_Z = 1;
			msopts = 1;
			break;
		case 'z':
			m->x11_md.n_display_names ++;
			CHECK_ALLOCATION(m->x11_md.display_names = (char **) realloc(
			    m->x11_md.display_names,
			    m->x11_md.n_display_names * sizeof(char *)));
			CHECK_ALLOCATION(m->x11_md.display_names[
			    m->x11_md.n_display_names-1] = strdup(optarg));
			msopts = 1;
			break;
		default:
			fprintf(stderr, "Run  %s -h  for help on command "
			    "line options.\n", progname);
			exit(1);
		}
	}

	argc -= optind;
	argv += optind;

	extra_argc = argc;
	extra_argv = argv;

	// If -V is used, -q is ignored.
	if (single_step == ENTER_SINGLE_STEPPING)
		quiet_mode = 0;

	if (type == NULL && subtype == NULL &&
	    (single_step == ENTER_SINGLE_STEPPING || argc > 0)) {
		int res2 = 0;
		{
			GXemul gxemul;
			gxemul.InitUI();

			if (single_step == ENTER_SINGLE_STEPPING)
				gxemul.SetRunState(GXemul::Paused);
			else
				gxemul.SetRunState(GXemul::Running);

			gxemul.SetSnapshottingEnabled(using_switch_B);

			if (quiet_mode)
				gxemul.SetQuietMode(true);

			if (argc > 0 && !gxemul.ParseFilenames("", argc, argv))
				res = 1;

			if (res2 == 0)
				res2 = gxemul.Run();
		}

		// Note: exit() is outside the GXemul scope, so that GXemul's
		// destructor runs.
		exit(res2);
	}

	if (type != NULL || subtype != NULL) {
		if (type == NULL)
			type = strdup("");
		if (subtype == NULL)
			subtype = strdup("");

		/*  Is it a new machine mode?  */
		if (subtype[0] != '\0') {
			int res2 = 0;
			bool doExit = false;
			
			{
				GXemul gxemul;
				gxemul.InitUI();

				if (single_step == ENTER_SINGLE_STEPPING)
					gxemul.SetRunState(GXemul::Paused);
				else
					gxemul.SetRunState(GXemul::Running);

				gxemul.SetSnapshottingEnabled(using_switch_B);

				if (quiet_mode)
					gxemul.SetQuietMode(true);

				if (gxemul.IsTemplateMachine(subtype)) {
					if (!gxemul.ParseFilenames(subtype, argc, argv))
						res2 = 1;

					if (res2 == 0)
						res2 = gxemul.Run();

					doExit = true;
				}
			}
			
			if (doExit)
				exit(res2);
		}

		/*  Legacy mode?  */
		res = machine_name_to_type(type, subtype,
		    &m->machine_type, &m->machine_subtype, &m->arch);
		if (!res)
			exit(1);
	}

	if (m->machine_type == MACHINE_NONE && msopts) {
		fprintf(stderr, "Machine specific options used directly on "
		    "the command line, but no machine\nemulation specified?\n");
		exit(1);
	}


	/*  -i and -r are pretty verbose:  */

	if (m->instruction_trace && !verbose) {
		fprintf(stderr, "Implicitly %sturning on -v, because"
		    " of -i\n", quiet_mode? "turning off -q and " : "");
		verbose = 1;
		quiet_mode = 0;
	}

	if (m->register_dump && !verbose) {
		fprintf(stderr, "Implicitly %sturning on -v, because"
		    " of -r\n", quiet_mode? "turning off -q and " : "");
		verbose = 1;
		quiet_mode = 0;
	}


	/*
	 *  Usually, an executable filename must be supplied.
	 *
	 *  However, it is possible to boot directly from a harddisk image
	 *  file. If no kernel is supplied, but a diskimage is being used,
	 *  then try to boot from disk.
	 */
	if (extra_argc == 0) {
		if (using_switch_d) {
			/*  Booting directly from a disk image...  */
		} else {
			usage(0);
			fprintf(stderr, "\nNo filename given. Aborting.\n");
			exit(1);
		}
	} else if (m->boot_kernel_filename[0] == '\0') {
		/*
		 *  Default boot_kernel_filename is "", which can be overriden
		 *  by the -j command line option.  If it is still "" here,
		 *  and we're not booting directly from a disk image, then
		 *  try to set it to the last part of the last file name
		 *  given on the command line. (Last part = the stuff after
		 *  the last slash.)
		 */
		char *s = extra_argv[extra_argc - 1];
		char *s2;

		s2 = strrchr(s, '/');
		if (s2 == NULL)
			s2 = s;
		else
			s2 ++;

		CHECK_ALLOCATION(m->boot_kernel_filename = strdup(s2));
	}

	if (m->n_gfx_cards < 0 || m->n_gfx_cards > 3) {
		fprintf(stderr, "Bad number of gfx cards (-Z).\n");
		exit(1);
	}

	if (!using_switch_Z && !m->x11_md.in_use)
		m->n_gfx_cards = 0;

	return 0;
}


/*
 *  main():
 *
 *  Two kinds of emulations are started from here:
 *
 *	o)  Simple emulations, using command line arguments, compatible with
 *	    earlier version of GXemul/mips64emul.
 *
 *	o)  Emulations set up by parsing special config files. (0 or more.)
 */
int main(int argc, char *argv[])
{
	/*  Setting constants:  */
	int constant_yes = 1;
	int constant_true = 1;
	int constant_no = 0;
	int constant_false = 0;

	struct emul *emul;
	int config_file = 0;

	char **diskimages = NULL;
	int n_diskimages = 0;
	int i;


	progname = argv[0];


	/*
	 *  Create the settings object, and add global settings to it:
	 *
	 *  Read-only "constants":     yes, no, true, false.
	 *  Global emulator settings:  verbose, single_step, ...
	 */
	global_settings = settings_new();

	settings_add(global_settings, "yes", 0, SETTINGS_TYPE_INT,
	    SETTINGS_FORMAT_YESNO, (void *)&constant_yes);
	settings_add(global_settings, "no", 0, SETTINGS_TYPE_INT,
	    SETTINGS_FORMAT_YESNO, (void *)&constant_no);
	settings_add(global_settings, "true", 0, SETTINGS_TYPE_INT,
	    SETTINGS_FORMAT_BOOL, (void *)&constant_true);
	settings_add(global_settings, "false", 0, SETTINGS_TYPE_INT,
	    SETTINGS_FORMAT_BOOL, (void *)&constant_false);

	/*  Read-only settings:  */
	settings_add(global_settings, "single_step", 0,
	    SETTINGS_TYPE_INT, SETTINGS_FORMAT_YESNO, (void *)&single_step);

	/*  Read/write settings:  */
	settings_add(global_settings, "force_debugger_at_exit", 1,
	    SETTINGS_TYPE_INT, SETTINGS_FORMAT_YESNO,
	    (void *)&force_debugger_at_exit);
	settings_add(global_settings, "verbose", 1,
	    SETTINGS_TYPE_INT, SETTINGS_FORMAT_YESNO, (void *)&verbose);
	settings_add(global_settings, "quiet_mode", 1,
	    SETTINGS_TYPE_INT, SETTINGS_FORMAT_YESNO, (void *)&quiet_mode);

	/*  Initialize all emulator subsystems:  */
	console_init();
	cpu_init();
	device_init();
	machine_init();
	timer_init();

	/*  Create a simple emulation setup:  */
	emul = emul_new(NULL);
	settings_add(global_settings, "emul", 1,
	    SETTINGS_TYPE_SUBSETTINGS, 0, emul->settings);

	get_cmd_args(argc, argv, emul, &diskimages, &n_diskimages);

	if (!skip_srandom_call) {
		struct timeval tv;
		gettimeofday(&tv, NULL);
		srandom(tv.tv_sec ^ getpid() ^ tv.tv_usec);
	}

	/*  Print startup message:  */
	debug("GXemul "VERSION"    "COPYRIGHT_MSG"\n"SECONDARY_MSG
	    "Read the source code and/or documentation for other Copyright "
	    "messages.\n\n");

	/*  Simple initialization, from command line arguments:  */
	if (emul->machines[0]->machine_type != MACHINE_NONE) {
		for (i=0; i<n_diskimages; i++)
			diskimage_add(emul->machines[0], diskimages[i]);

		/*  Make sure that there are no configuration files as well:  */
		for (i=1; i<argc; i++)
			if (argv[i][0] == '@') {
				fprintf(stderr, "You can either start one "
				    "emulation with one machine directly from "
				    "the command\nline, or start one or more "
				    "emulations using configuration files."
				    " Not both.\n");
				exit(1);
			}

		/*  Initialize one emul:  */
		emul_simple_init(emul);
	}

	/*  Initialize an emulation from a config file:  */
	for (i=1; i<argc; i++) {
		if (argv[i][0] == '@') {
			char *s = argv[i] + 1;

			if (config_file) {
				fprintf(stderr, "More than one configuration "
				    "file cannot be used.\n");
				exit(1);
			}

			if (strlen(s) == 0 && i+1 < argc && *argv[i+1] != '@')
				s = argv[++i];

			/*  Always allow slave xterms:  */
			console_allow_slaves(1);

			/*  Destroy the temporary emul, since it will
			    be overwritten:  */
			if (emul != NULL) {
				emul_destroy(emul);
				settings_remove(global_settings, "emul");
				emul = NULL;
			}

			emul = emul_create_from_configfile(s);

			settings_add(global_settings, "emul", 1,
			    SETTINGS_TYPE_SUBSETTINGS, 0, emul->settings);

			config_file = 1;
		}
	}

	if (emul->n_machines == 0) {
		fprintf(stderr, "No emulations defined. Maybe you forgot to "
		    "use -E xx and/or -e yy, to specify\nthe machine type."
		    " For example:\n\n    %s -e 3max -d disk.img\n\n"
		    "to boot an emulated DECstation 5000/200 with a disk "
		    "image.\n", progname);
		exit(1);
	}

	if (emul->machines[0]->machine_type == MACHINE_NONE) {
		printf("No machine type specified? Run  gxemul -H  for a list\n"
		    "of available machine types. Use the -e or -E option(s)\n"
		    "to specify the machine type.\n");
		exit(1);
	}

	device_set_exit_on_error(0);
	console_warn_if_slaves_are_needed(1);


	/*  Run the emulation:  */
	emul_run(emul);


	/*
	 *  Deinitialize everything:
	 */

	console_deinit();

	emul_destroy(emul);

	settings_remove_all(global_settings);
	settings_destroy(global_settings);

	return 0;
}

