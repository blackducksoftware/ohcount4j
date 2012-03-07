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

public class OhcountDetector extends Detector {

	private static OhcountDetector instance = new OhcountDetector();

	private OhcountDetector() {

		extension("c").scanUsing(CScanner.class);
		extension("cs").scanUsing(CSharpScanner.class);
		extension("css").scanUsing(CSSScanner.class);
		extensions("htm", "html").scanUsing(HTMLScanner.class);
		extension("java").scanUsing(JavaScanner.class);
		extension("js").scanUsing(JavaScriptScanner.class);
		extensions("rb", "ru").scanUsing(RubyScanner.class);
		extension("sql").scanUsing(SqlScanner.class);
		extension("sh").scanUsing(ShellScanner.class);
		extension("xml").scanUsing(XmlScanner.class);

		name("Makefile").scanUsing(MakefileScanner.class);
		names("Rakefile", "Gemfile").scanUsing(RubyScanner.class);

		extension("h").resolveUsing(ExtnHResolver.class);
	}

	public static Detector getInstance() {
		return instance;
	}

}
