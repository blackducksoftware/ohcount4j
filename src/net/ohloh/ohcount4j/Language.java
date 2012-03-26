package net.ohloh.ohcount4j;

import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.scan.*;

public enum Language {

	/* All languages must be defined here.
	 *
	 * Each language must declare two mandatory properties:
	 *
	 *  - The language's official display name (niceName)
	 *  - A Scanner subclass capable of parsing this language
	 */
	ACTIONSCRIPT("ActionScript", ActionScriptScanner.class),
	ADA("Ada", AdaScanner.class),
	ASPX_CSHARP("ASP.NET (C#)", GenericCodeScanner.class), // TODO.
	ASPX_VB("ASP.NET (VB)", GenericCodeScanner.class), // TODO.
	ASSEMBLY("Assembly", AssemblyScanner.class),
	BINARY("Binary", null), // Place holder for binary files. This "Language" will not trigger a scan.
	BOO("Boo", BooScanner.class),
	C("C", CStyleScanner.class),
	COBOL("COBOL", CobolScanner.class),
	COLDFUSION("ColdFusion", ColdFusionScanner.class),
	CPP("C++", CStyleScanner.class),
	CSHARP("C#", CStyleScanner.class),
	CSS("CSS", CStyleScanner.class),
	ECMASCRIPT("ECMAScript", CStyleScanner.class),
	EIFFEL("Eiffel", EiffelScanner.class),
	ERLANG("Erlang", ErlangScanner.class),
	FSHARP("F#", FSharpScanner.class),
	GROOVY("Groovy", CStyleScanner.class),
	HTML("HTML", HTMLScanner.class),
	HASKELL("Haskell", HaskellScanner.class),
	JAVA("Java", CStyleScanner.class),
	JAVASCRIPT("JavaScript", CStyleScanner.class),
	LIMBO("Limbo", CStyleScanner.class),
	JSP("JSP", JspScanner.class),
	LISP("Lisp", LispScanner.class),
	LUA("Lua", LuaScanner.class),
	MAKE("Make", MakeScanner.class),
	MATLAB("Matlab", MatlabScanner.class),
	MODULA2("Modula 2", ModulaScanner.class),
	MODULA3("Modula 3", ModulaScanner.class),
	OBJECTIVE_C("Objective-C", CStyleScanner.class),
	OCAML("OCaml", OCamlScanner.class),
	OCTAVE("Octave", MatlabScanner.class), // TODO. Octave also supports # comments
	PASCAL("Pascal", PascalScanner.class),
	PERL("Perl", PerlScanner.class),
	PHP("Php", PhpScanner.class),
	PUPPET("Puppet", GenericCodeScanner.class), // TODO.
	PVWAVE("IDL/PV-WAVE/GDL", GenericCodeScanner.class), // TODO.
	PROLOG("Prolog", PrologScanner.class),
	PYTHON("Python", PythonScanner.class),
	R("R", GenericCodeScanner.class), // TODO.
	REBOL("REBOL", RebolScanner.class),
	REXX("Rexx", RexxScanner.class),
	RUBY("Ruby", RubyScanner.class),
	SCHEME("Scheme", SchemeScanner.class),
	SHELL("Shell", ShellScanner.class),
	SMALLTALK("Smalltalk", SmalltalkScanner.class),
	SQL("SQL", SqlScanner.class),
	TCL("Tcl", TclScanner.class),
	VB("VisualBasic", VisualBasicScanner.class),
	XML("XML", XmlScanner.class);

	/* Optional properties of languages are declared here.
	 *
	 * At a minimum, a language should define one or more file
	 * extensions or filenames associated with the language.
	 *
	 * You may also declare additional names (beyond the uname
	 * and niceName) by which the language might be known.
	 * These aliases can be matched against things like Emacs
	 * mode headers or shebang directives.
	 */
	static {
		ACTIONSCRIPT.extension("as");
		ADA.extensions("ada", "adb");
		ASPX_CSHARP.extension("aspx");
		ASPX_VB.extension("aspx");
		ASSEMBLY.extensions("as8", "asm", "asx", "S", "z80");
		BINARY.extension("inc");
		BOO.extension("boo");
		C.extensions("c", "h");
		COBOL.extension("cbl");
		COLDFUSION.extensions("cfc", "cfm");
		CPP.extensions("C", "c++", "cc", "cpp", "cxx", "H", "h", "h++", "hh", "hpp", "hxx");
		CSHARP.aliases("C#", "cs").extension("cs");
		CSS.extension("css");
		ECMASCRIPT.extension("es");
		EIFFEL.extension("e");
		ERLANG.extension("erl");
		FSHARP.extension("fs");
		GROOVY.extension("groovy");
		HTML.extensions("htm", "html");
		HASKELL.extensions("hs", "lhs");
		JAVA.extension("java");
		JAVASCRIPT.alias("js").extension("js");
		JSP.extension("jsp");
		LIMBO.extensions("b", "m");
		LUA.extension("lua");
		MAKE.filename("Makefile").extensions("mk", "pro");
		MODULA2.extensions("mod", "m2");
		MODULA3.extensions("m3", "i3");
		OBJECTIVE_C.extensions("m", "h");
		OCAML.extensions("ml", "mli");
		OCTAVE.extensions("m", "octave");
		PASCAL.extensions("pas", "pp");
		PERL.extensions("pl", "pm");
		PHP.extensions("inc", "php", "phtml", "php4", "php3", "php5", "phps");
		PVWAVE.extension("pro");
		PROLOG.extension("pl");
		PUPPET.extension("pp");
		PYTHON.extension("py");
		R.extension("r");
		REBOL.extensions("r", "r3", "reb", "rebol");
		REXX.extensions("cmd", "exec", "rexx");
		RUBY.alias("jruby").extensions("rb", "ru").filenames("Rakefile", "Gemfile");
		SCHEME.extensions("scm", "ss");
		SHELL.extensions("bash", "sh");
		SMALLTALK.extension("st");
		SQL.extension("sql");
		TCL.extension("tcl");
		VB.extension("vb");
		XML.extensions("asx", "csproj", "xml", "mxml");
	}

	private final String niceName;
	private final Class<? extends Scanner> scannerClass;
	private List<String> extensions;
	private List<String> filenames;
	private List<String> aliases;

	Language(String niceName, Class<? extends Scanner> scannerClass) {
		this.niceName = niceName;
		this.scannerClass = scannerClass;
		this.extensions = new ArrayList<String>();
		this.filenames = new ArrayList<String>();
		this.aliases = new ArrayList<String>();
	}

	public String uname() {
		return this.toString().toLowerCase();
	}

	public String niceName() {
		return niceName;
	}

	public Class<? extends Scanner> scannerClass() {
		return scannerClass;
	}

	public Scanner makeScanner() {
		try {
			Scanner scanner = this.scannerClass.newInstance();
			scanner.setDefaultLanguage(this);
			return scanner;
		} catch (InstantiationException e) {
			throw new OhcountException(e);
		} catch (IllegalAccessException e) {
			throw new OhcountException(e);
		}
	}

	public Language extension(String ext) {
		extensions.add(ext);
		return this;
	}

	public Language extensions(String... exts) {
		for (String ext : exts) {
			extension(ext);
		}
		return this;
	}

	public List<String> getExtensions() {
		return extensions;
	}

	public Language filename(String filename) {
		filenames.add(filename);
		return this;
	}

	public Language filenames(String... filenames) {
		for (String filename : filenames) {
			filename(filename);
		}
		return this;
	}

	public List<String> getFilenames() {
		return filenames;
	}

	public Language alias(String alias) {
		aliases.add(alias);
		return this;
	}

	public Language aliases(String... aliases) {
		for (String alias : aliases) {
			alias(alias);
		}
		return this;
	}

	public List<String> getAliases() {
		return aliases;
	}
}
