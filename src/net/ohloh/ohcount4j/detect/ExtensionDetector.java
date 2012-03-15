package net.ohloh.ohcount4j.detect;

import java.util.HashMap;
import java.util.Map;

import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;
import net.ohloh.ohcount4j.scan.*;

public class ExtensionDetector {

	private static Map<String, Class<? extends Scanner>> fixedMap = null;
	private static Map<String, Resolver> ambiguousMap = null;

	// Returns a dictionary of file extensions for which there is no doubt
	// which Scanner we will use.
	private static Map<String, Class<? extends Scanner>> getFixedMap() {
		return fixedMap;
	}

	// Returns a dictionary of file extensions which are associated with more
	// then one possible Scanner (for instance, *.h may be C, C++, or Objective-C).
	//
	// Instead of Scanners, this dictionary contains Resolvers, which
	// must inspect the file contents to determine a final Scanner.
	private static Map<String, Resolver> getAmbiguousMap() {
		if (ambiguousMap == null) {
			ambiguousMap = new HashMap<String, Resolver>();

			ambiguousMap.put("h", new ExtnHResolver());
		}
		return ambiguousMap;
	}

	public static Class<? extends Scanner> detect(String extension, Source blob) throws OhcountException {
		Class<? extends Scanner> klass = getFixedMap().get(extension);
		if (klass == null) {
			Resolver resolver = getAmbiguousMap().get(extension);
			if (resolver != null) {
				klass = resolver.resolve(blob);
			}
		}
		return klass;
	}

}
