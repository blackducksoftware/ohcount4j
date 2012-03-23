package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnPROResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		if (qmakePattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.MAKE; // Actually QMAKE. Should this be a distinct language?
		} else {
			return Language.PVWAVE;
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.MAKE ||
			language == Language.PVWAVE) {
			return true;
		} else {
			return false;
		}
	}

	private static Pattern qmakePattern = Pattern.compile("\\b(SOURCES|CONFIG)\\s*\\+\\=");
}
