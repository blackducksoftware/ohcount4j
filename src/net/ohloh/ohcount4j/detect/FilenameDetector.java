package net.ohloh.ohcount4j.detect;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.scan.MakefileScanner;
import net.ohloh.ohcount4j.scan.RubyScanner;
import net.ohloh.ohcount4j.scan.Scanner;

public class FilenameDetector {
	private static Map<String, Class<? extends Scanner>> filenameMap = null;

	private static Map<String, Class<? extends Scanner>> getFilenameMap() {
		if (filenameMap == null) {
			filenameMap = new HashMap<String, Class<? extends Scanner>>();

			filenameMap.put("Gemfile",  RubyScanner.class);
			filenameMap.put("Makefile", MakefileScanner.class);
			filenameMap.put("Rakefile", RubyScanner.class);
		}
		return filenameMap;
	}

	public static Class<? extends Scanner> detect(String filename) {
		return getFilenameMap().get(filename);
	}
}