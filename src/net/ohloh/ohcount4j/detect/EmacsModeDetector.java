package net.ohloh.ohcount4j.detect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.scan.Scanner;

public class EmacsModeDetector {

	protected static Pattern pattern =
			Pattern.compile("-\\*-\\s*(mode:)?\\s*([^\\s;]+);?\\s*-\\*-",
			Pattern.CASE_INSENSITIVE);

	// Given a string representing the initial portion of a file,
	// check for an Emacs mode header declaration and return its mode.
	public static String getMode(String s) {
		if (s == null) {
			return null;
		}

		Matcher matcher =  pattern.matcher(s);
		if (matcher.find()) {
			return matcher.group(2);
		} else {
	        return null;
		}
	}

	// Inspect the buffer for a possible Emacs mode header.
	// Returns a Scanner if a mode header is recognized, otherwise null.
	public static Class<? extends Scanner> detect(String buffer) {
		String mode = getMode(buffer);

		return LanguageNameDetector.detect(mode);
	}
}
