package net.ohloh.ohcount4j.detect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.scan.Scanner;

public class MagicDetector {

	// Use libmagic to identify the buffer contents.
	// Returns a Scanner class if the file type is recognized, otherwise null.
	public static Class<? extends Scanner> detect(String buffer) throws OhcountException {
		String description = getMagicDescription(buffer);
		String languageName = getLanguageName(description);

		Language language = Language.fromName(languageName);
		if (language != null) {
			return language.scannerClass();
		} else {
			return null;
		}
	}

	protected static Pattern patterns[] = {
		Pattern.compile("^script text(?: executable)? for (\\w+)", 0),
		Pattern.compile("(\\w+)(?: -\\w+)* script(?:, \\w+)? text", 0),
		Pattern.compile("(\\w+) program text", 0),
	};

	public static String getLanguageName(String description) throws OhcountException {
		if (description == null) {
			return null;
		}

		for (Pattern pattern : patterns) {
			Matcher matcher =  pattern.matcher(description);
			if (matcher.find()) {
				return matcher.group(1);
			}
		}
		return null;
	}

	public static String getMagicDescription(String buffer) throws OhcountException {
		if (buffer == null) {
			return null;
		}

		Magic magic = new Magic();

		magic.open();
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		magic.load();
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		String description = magic.buffer(buffer);
		if (magic.error() != null) {
			throw new OhcountException(magic.error());
		}

		magic.close();

		return description;
	}

}