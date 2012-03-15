package net.ohloh.ohcount4j;

public enum Language {
	LANG_ACTIONSCRIPT("actionscript", "ActionScript"),
	LANG_ADA("ada", "Ada"),
	LANG_ASM("asm", "Assembly"),
	LANG_BOO("boo", "Boo"),
	LANG_C("c", "C"),
	LANG_CPP("cpp", "C++"),
	LANG_CSHARP("cs", "C#"),
	LANG_CSS("css", "CSS"),
	LANG_EIFFEL("eiffel", "Eiffel"),
	LANG_ERLANG("erlang", "Erlang"),
	LANG_FSHARP("fs", "F#"),
	LANG_GROOVY("groovy", "Groovy"),
	LANG_HTML("html", "HTML"),
	LANG_JAVA("java", "Java"),
	LANG_JAVASCRIPT("javascript", "JavaScript"),
	LANG_LISP("lisp", "Lisp"),
	LANG_LUA("lua", "Lua"),
	LANG_MAKEFILE("make", "Make"),
	LANG_MATLAB("matlab", "Matlab"),
	LANG_OBJECTIVEC("objectivec", "Objective-C"),
	LANG_PASCAL("pascal", "Pascal"),
	LANG_PROLOG("prolog", "Prolog"),
	LANG_PYTHON("python", "Python"),
	LANG_REBOL("rebol", "REBOL"),
	LANG_RUBY("ruby", "Ruby"),
	LANG_SHELL("shellscript", "ShellScript"),
	LANG_SMALLTALK("smalltalk", "Smalltalk"),
	LANG_SQL("sql", "SQL"),
	LANG_TCL("tcl", "Tcl"),
	LANG_VB("vb", "VisualBasic"),
	LANG_XML("xml", "XML");

	private final String uname;
	private final String niceName;

	Language(String uname, String niceName) {
		this.uname = uname;
		this.niceName = niceName;
	}

	public String uname() {
		return uname;
	}

	public String niceName() {
		return niceName;
	}
}
