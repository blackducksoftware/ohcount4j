package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnMResolver implements Resolver {

	@Override
	public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
		int scoreObjectiveC = countObjectiveC(sourceFile);
		int scoreMatlab = countMatlab(sourceFile);
		int scoreLimbo = countLimbo(sourceFile);

		if (scoreLimbo > scoreMatlab && scoreLimbo > scoreObjectiveC) {
			return Language.LIMBO;
		} else if (scoreObjectiveC > scoreMatlab) {
			return Language.OBJECTIVE_C;
		} else if (containsOctave(sourceFile)) {
			return Language.OCTAVE;
		} else {
			return Language.MATLAB;
		}
	}

	@Override
	public Language resolve(SourceFile sourceFile) throws IOException {
		return resolve(sourceFile, new ArrayList<String>());
	}

	@Override
	public boolean canResolve(Language language) {
		if (language == Language.LIMBO ||
			language == Language.MATLAB ||
			language == Language.OCTAVE ||
			language == Language.OBJECTIVE_C) {
			return true;
		}
		return false;
	}

	// A Limbo-style '#' line comment, or a Limbo declaration
	private static Pattern limboPattern = Pattern.compile(
			"(^\\s*\\#\\s+|:\\s+(?:module|adt|fn\\s*\\(|con\\s+))",
			Pattern.MULTILINE);

	public int countLimbo(SourceFile sourceFile) throws IOException {
		return countMatches(limboPattern, sourceFile);
	}

	// Matlab and Octave are very similar.
	// Octave can be distinguished from Matlab by its '#' line comments,
	// and a few keywords which do not appear in Matlab.
	private static Pattern octavePattern = Pattern.compile(
			"(^\\s*\\#\\s+|\\b(?:end_try_catch|end_unwind_protect|endfunction|endwhile)\\b)",
			Pattern.MULTILINE);

	public int countOctave(SourceFile sourceFile) throws IOException {
		return countMatches(octavePattern, sourceFile);
	}

	public boolean containsOctave(SourceFile sourceFile) throws IOException {
		Matcher m = octavePattern.matcher(sourceFile.getCharSequence());
		return m.find();
	}

	// An Objective-C style '//' line comment, or an Objective-C keyword
	public static Pattern objectiveCPattern = Pattern.compile(
			"^\\s*(?://|\\-|@interface|@implementation|#import)",
			Pattern.MULTILINE);

	public int countObjectiveC(SourceFile sourceFile) throws IOException {
		return countMatches(objectiveCPattern, sourceFile);
	}

	// A Matlab-style '%' line comment, or a Matlab keyword
	public static Pattern matlabPattern = Pattern.compile(
			"^\\s*(%\\s+|function\\b)",
			Pattern.MULTILINE);

	public int countMatlab(SourceFile sourceFile) throws IOException {
		return countMatches(matlabPattern, sourceFile);
	}

	private int countMatches(Pattern p, SourceFile sourceFile) throws IOException {
		Matcher m = p.matcher(sourceFile.getCharSequence());
		int result = 0;
		int mark = 0;
		while (m.find(mark)) {
			result++;
			mark = m.end();
		}
		return result;
	}
}