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
import net.ohloh.ohcount4j.scan.AdaScanner;

public class OhcountDetector extends Detector {

	private static OhcountDetector instance = new OhcountDetector();

	private OhcountDetector() {

		// Map extensions
		extension("c").scanUsing(CScanner.class);
		extension("html").scanUsing(HTMLScanner.class);
		extension("css").scanUsing(CSSScanner.class);
		extension("java").scanUsing(JavaScanner.class);
		extension("rb").scanUsing(RubyScanner.class);
		extension("js").scanUsing(JavaScriptScanner.class);
		extension("xml").scanUsing(XmlScanner.class);
		extension("cs").scanUsing(CSharpScanner.class);
		extension("sql").scanUsing(SqlScanner.class);
		extension("sh").scanUsing(ShellScanner.class);
		extension("adb").scanUsing(AdaScanner.class);
		extension("ads").scanUsing(AdaScanner.class);
		//extension("a").scanUsing(AdaScanner.class);
		// map(extension())

		// Dummy mapping
		extensions("x", "y", "z").scanUsing(CScanner.class);

		name("Makefile").scanUsing(MakefileScanner.class);

		// Dummy name mapping
		names("Rakefile", "Gemfile", "config.ru").scanUsing(CScanner.class);

		// Map ambiguous extensions
		extension("h").resolveUsing(ExtnHResolver.class);
	}

	public static Detector getInstance() {
		return instance;
	}

}
