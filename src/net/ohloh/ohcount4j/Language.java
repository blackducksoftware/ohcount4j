package net.ohloh.ohcount4j;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.scan.*;

public enum Language {
	LANG_ACTIONSCRIPT("actionscript", "ActionScript", ActionScriptScanner.class),
	LANG_ADA("ada", "Ada", AdaScanner.class),
	LANG_ASM("asm", "Assembly", AssemblyScanner.class),
	LANG_BOO("boo", "Boo", BooScanner.class),
	LANG_C("c", "C", CScanner.class),
	LANG_CSHARP("cs", "C#", CSharpScanner.class),
	LANG_CSS("css", "CSS", CSSScanner.class),
	LANG_EIFFEL("eiffel", "Eiffel", EiffelScanner.class),
	LANG_ERLANG("erlang", "Erlang", ErlangScanner.class),
	LANG_FSHARP("fs", "F#", FSharpScanner.class),
	LANG_GROOVY("groovy", "Groovy", GroovyScanner.class),
	LANG_HTML("html", "HTML", HTMLScanner.class),
	LANG_JAVA("java", "Java", JavaScanner.class),
	LANG_JAVASCRIPT("javascript", "JavaScript", JavaScriptScanner.class),
	LANG_LISP("lisp", "Lisp", LispScanner.class),
	LANG_LUA("lua", "Lua", LuaScanner.class),
	LANG_MAKEFILE("make", "Make", MakefileScanner.class),
	LANG_MATLAB("matlab", "Matlab", MatlabScanner.class),
	LANG_OBJECTIVEC("objectivec", "Objective-C", ObjectiveCScanner.class),
	LANG_PASCAL("pascal", "Pascal", PascalScanner.class),
	LANG_PROLOG("prolog", "Prolog", PrologScanner.class),
	LANG_PYTHON("python", "Python", PythonScanner.class),
	LANG_REBOL("rebol", "REBOL", RebolScanner.class),
	LANG_RUBY("ruby", "Ruby", RubyScanner.class),
	LANG_SHELL("shellscript", "ShellScript", ShellScanner.class),
	LANG_SMALLTALK("smalltalk", "Smalltalk", SmalltalkScanner.class),
	LANG_SQL("sql", "SQL", SqlScanner.class),
	LANG_TCL("tcl", "Tcl", TclScanner.class),
	LANG_VB("vb", "VisualBasic", VisualBasicScanner.class),
	LANG_XML("xml", "XML", XmlScanner.class);

	private final String uname;
	private final String niceName;
	private final Class <? extends Scanner> scannerClass;

	private static Map<String, Language> extensionMap;
	private static Map<String, Language> filenameMap;
	private static Map<String, Language> nameMap;

	static {
		extensionMap = new HashMap<String, Language>();
		filenameMap = new HashMap<String, Language>();
		nameMap = new HashMap<String, Language>();

		for (Language language : Language.values()) {
			nameMap.put(language.uname().toLowerCase(), language);
			nameMap.put(language.niceName().toLowerCase(), language);
		}

		LANG_ACTIONSCRIPT
			.extension("as")
			;

		LANG_ADA
			.extension("ada")
			.extension("adb")
			;

		LANG_ASM
			.extension("asm")
			;

		LANG_BOO
			.extension("boo")
			;

		LANG_C
			.extension("c")
			;

		LANG_CSHARP
			.alias("C#")
			.extension("cs")
			;

		LANG_CSS
			.extension("css")
			;

		LANG_ERLANG
			.extension("erl")
			;

		LANG_FSHARP
			.alias("F#")
			.extension("fs")
			;

		LANG_GROOVY
			.extension("groovy")
			;

		LANG_HTML
			.extension("html")
			.extension("htm")
			;

		LANG_JAVA
			.extension("java")
			;

		LANG_JAVASCRIPT
			.alias("js")
			.extension("js")
			;

		LANG_LUA
			.extension("lua")
			;

		LANG_MAKEFILE
			.filename("Makefile")
			;

		LANG_OBJECTIVEC
			.extension("m")
			;

		LANG_PASCAL
			.extension("pas")
			;

		LANG_PROLOG
			.extension("pl")
			;

		LANG_PYTHON
			.extension("py")
			;

		LANG_REBOL
			.extension("r")
			;

		LANG_RUBY
			.alias("jruby")
			.extension("rb")
			.extension("ru")
			.filename("Rakefile")
			.filename("Gemfile")
			;

		LANG_SQL
			.extension("sql")
			;

		LANG_SMALLTALK
			.extension("st")
			;

		LANG_SHELL
			.extension("bash")
			.extension("sh")
			;

		LANG_TCL
			.extension("tcl")
			;

		LANG_VB
			.extension("vb")
			;

		LANG_XML
			.extension("xml")
			;
	}

	Language(String uname, String niceName, Class<? extends Scanner> scannerClass) {
		this.uname = uname;
		this.niceName = niceName;
		this.scannerClass = scannerClass;
	}

	public String uname() {
		return uname;
	}

	public String niceName() {
		return niceName;
	}

	public Class<? extends Scanner> scannerClass() {
		return scannerClass;
	}

	public Language extension(String ext) {
		extensionMap.put(ext, this);
		return this;
	}

	public static Language fromExtension(String ext) {
		return extensionMap.get(ext);
	}

	public Language filename(String filename) {
		filenameMap.put(filename, this);
		return this;
	}

	public static Language fromFilename(String filename) {
		return filenameMap.get(filename);
	}

	public Language alias(String alias) {
		nameMap.put(alias, this);
		return this;
	}

	public static Language fromName(String name) {
		if (name != null) {
			return nameMap.get(name.toLowerCase());
		} else {
			return null;
		}
	}
}
