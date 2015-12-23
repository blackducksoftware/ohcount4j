package net.ohloh.ohcount4j.detect;

import static java.util.regex.Pattern.MULTILINE;
import static net.ohloh.ohcount4j.Language.LIMBO;
import static net.ohloh.ohcount4j.Language.MATLAB;
import static net.ohloh.ohcount4j.Language.OBJECTIVE_C;
import static net.ohloh.ohcount4j.Language.OCTAVE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnMResolver extends AbstractExtnResolver {

    // A Limbo-style '#' line comment, or a Limbo declaration
    private static final Pattern LIMBO_PATTERN = Pattern.compile("(^\\s*\\#\\s+|:\\s+(?:module|adt|fn\\s*\\(|con\\s+))", MULTILINE);

    // Matlab and Octave are very similar.
    // Octave can be distinguished from Matlab by its '#' line comments,
    // and a few keywords which do not appear in Matlab.
    private static final Pattern OCTAVE_PATTERN = Pattern.compile("(^\\s*\\#\\s+|\\b(?:end_try_catch|end_unwind_protect|endfunction|endwhile)\\b)", MULTILINE);

    // An Objective-C style '//' line comment, or an Objective-C keyword
    public static final Pattern OBJECTIVE_PATTERN = Pattern.compile("^\\s*(?://|\\-|@interface|@implementation|#import)", MULTILINE);

    // A Matlab-style '%' line comment, or a Matlab keyword
    public static final Pattern MATLAB_PATTERN = Pattern.compile("^\\s*(%\\s+|function\\b)", MULTILINE);

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
        if (language == LIMBO || language == MATLAB || language == OCTAVE || language == OBJECTIVE_C) {
            return true;
        }
        return false;
    }

    public int countLimbo(SourceFile sourceFile) throws IOException {
        return countMatches(LIMBO_PATTERN, sourceFile);
    }

    public int countOctave(SourceFile sourceFile) throws IOException {
        return countMatches(OCTAVE_PATTERN, sourceFile);
    }

    public boolean containsOctave(SourceFile sourceFile) throws IOException {
        Matcher m = OCTAVE_PATTERN.matcher(sourceFile.getCharSequence());
        return m.find();
    }

    public int countObjectiveC(SourceFile sourceFile) throws IOException {
        return countMatches(OBJECTIVE_PATTERN, sourceFile);
    }

    public int countMatlab(SourceFile sourceFile) throws IOException {
        return countMatches(MATLAB_PATTERN, sourceFile);
    }

    private int countMatches(Pattern p, SourceFile sourceFile) throws IOException {
        Matcher m = p.matcher(getCharSequence(sourceFile));
        int result = 0;
        int mark = 0;
        while (m.find(mark)) {
            result++;
            mark = m.end();
        }
        return result;
    }

}
