package net.ohloh.ohcount4j;

public enum Language {
	LANG_C("c", "C"),
	LANG_CPP("cpp", "C++"),
	LANG_CSS("css", "CSS"),
	LANG_HTML("html", "HTML"),
	LANG_JAVA("java", "Java"),
	LANG_JAVASCRIPT("javascript", "JavaScript"),
	LANG_MAKEFILE("make", "Make"),
	LANG_RUBY("ruby", "Ruby"),
	LANG_XML("xml", "XML"),
	LANG_CSHARP("cs", "C#"),
	LANG_SQL("sql", "SQL"),
	LANG_SHELL("shellscript", "ShellScript"),
	LANG_VB("vb", "VisualBasic");
	LANG_SMALLTALK("smalltalk", "Smalltalk");
	LANG_REBOL("rebol", "REBOL");
	LANG_TCL("tcl", "Tcl");
	LANG_PYTHON("python", "Python");
	LANG_PROLOG("prolog", "Prolog");
	LANG_PASCAL("pascal", "Pascal");
	LANG_OBJECTIVEC("objectivec", "Objective-C");
	LANG_MATLAB("matlab", "Matlab");
	LANG_LUA("lua", "Lua");

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
