package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnPLResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		if (perlShebangPattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.PERL;
		}
		if (prologRulePattern.matcher(sourceFile.getCharSequence()).find()) {
			return Language.PROLOG;
		}
		return Language.PERL;
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.PERL ||
			language == Language.PROLOG) {
			return true;
		} else {
			return false;
		}
	}

	private static Pattern perlShebangPattern = Pattern.compile(
			"^\\#\\!.*\\bperl\\b", Pattern.CASE_INSENSITIVE);

	private static Pattern prologRulePattern = Pattern.compile(
			"\\:\\-\\s+", Pattern.MULTILINE);
}