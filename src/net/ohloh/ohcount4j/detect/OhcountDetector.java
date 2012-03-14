package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.CSSScanner;
import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.HTMLScanner;
import net.ohloh.ohcount4j.scan.JavaScanner;
import net.ohloh.ohcount4j.scan.MakefileScanner;
import net.ohloh.ohcount4j.scan.RubyScanner;
import net.ohloh.ohcount4j.scan.JavaScriptScanner;
import net.ohloh.ohcount4j.scan.SqlScanner;
import net.ohloh.ohcount4j.scan.XmlScanner;
import net.ohloh.ohcount4j.scan.CSharpScanner;
import net.ohloh.ohcount4j.scan.ShellScanner;
<<<<<<< HEAD
import net.ohloh.ohcount4j.scan.VisualBasicScanner;
import net.ohloh.ohcount4j.scan.SmalltalkScanner;
import net.ohloh.ohcount4j.scan.RebolScanner;
import net.ohloh.ohcount4j.scan.TclScanner;
import net.ohloh.ohcount4j.scan.PythonScanner;
import net.ohloh.ohcount4j.scan.PrologScanner;
import net.ohloh.ohcount4j.scan.PascalScanner;
import net.ohloh.ohcount4j.scan.ObjectiveCScanner;
import net.ohloh.ohcount4j.scan.MatlabScanner;
import net.ohloh.ohcount4j.scan.LuaScanner;
import net.ohloh.ohcount4j.scan.LispScanner;
import net.ohloh.ohcount4j.scan.GroovyScanner;

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

		name("Makefile").scanUsing(MakefileScanner.class);
		names("Rakefile", "Gemfile").scanUsing(RubyScanner.class);

		extension("h").resolveUsing(ExtnHResolver.class);
	}

	public static Detector getInstance() {
		return instance;
	}

}
