package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.*;

public class OhcountDetector extends Detector {

	private static OhcountDetector instance = new OhcountDetector();

	private OhcountDetector() {

		extension("c").scanUsing(CScanner.class);
		extension("cs").scanUsing(CSharpScanner.class);
		extension("css").scanUsing(CSSScanner.class);
		extensions("htm", "html").scanUsing(HTMLScanner.class);
		extension("java").scanUsing(JavaScanner.class);
		extension("js").scanUsing(JavaScriptScanner.class);
		extension("py").scanUsing(PythonScanner.class);
		extensions("rb", "ru").scanUsing(RubyScanner.class);
		extension("sh").scanUsing(ShellScanner.class);
		extension("sql").scanUsing(SqlScanner.class);
		extension("xml").scanUsing(XmlScanner.class);
		extension("vb").scanUsing(VisualBasicScanner.class);
		extension("st").scanUsing(SmalltalkScanner.class);
		extension("r").scanUsing(RebolScanner.class);
		extension("tcl").scanUsing(TclScanner.class);
		extension("pl").scanUsing(PrologScanner.class);
		extension("pas").scanUsing(PascalScanner.class);
		extension("m").scanUsing(ObjectiveCScanner.class);
		// extension("m").scanUsing(MatlabScanner.class); // TODO Disambiguate from Objective-C
		extension("lua").scanUsing(LuaScanner.class);
		extension("lisp").scanUsing(LispScanner.class);
		extension("groovy").scanUsing(GroovyScanner.class);
		extension("fs").scanUsing(FSharpScanner.class);
		extension("erl").scanUsing(ErlangScanner.class);
		extension("e").scanUsing(EiffelScanner.class);
		extension("boo").scanUsing(BooScanner.class);
		extension("asm").scanUsing(AssemblyScanner.class);
		extension("adb").scanUsing(AdaScanner.class);
		extension("ads").scanUsing(AdaScanner.class);
		//extension("a").scanUsing(AdaScanner.class);
		extension("as").scanUsing(ActionScriptScanner.class);

		name("Makefile").scanUsing(MakefileScanner.class);
		names("Rakefile", "Gemfile").scanUsing(RubyScanner.class);

		extension("h").resolveUsing(ExtnHResolver.class);
	}

	public static Detector getInstance() {
		return instance;
	}

}
