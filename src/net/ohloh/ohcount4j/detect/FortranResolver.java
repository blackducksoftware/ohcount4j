package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class FortranResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		if (freePattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.FORTRAN_FREE;
		} else {
			return Language.FORTRAN_FIXED;
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.FORTRAN_FIXED ||
			language == Language.FORTRAN_FREE) {
			return true;
		} else {
			return false;
		}
	}

	// Definitely Free format if non-comment, non-digit char in first 5 columns
	private static Pattern freePattern = Pattern.compile("^[Cc!\\s0-9]{0,4}[^Cc!\\s0-9]",
			Pattern.MULTILINE);

	// 
}
