package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.HTMLScanner;
import net.ohloh.ohcount4j.scan.MakefileScanner;

public class OhcountDetector extends Detector {

	private static OhcountDetector instance = new OhcountDetector();

	private OhcountDetector() {

		// Map extensions
		extension("c").scanUsing(CScanner.class);
		extension("html").scanUsing(HTMLScanner.class);
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
