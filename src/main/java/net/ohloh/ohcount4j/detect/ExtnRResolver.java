package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnRResolver extends AbstractExtnResolver {

    private static final Pattern REBOL_PATTERN = Pattern.compile("\\bREBOL\\b", Pattern.CASE_INSENSITIVE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (REBOL_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.REBOL;
        } else {
            return Language.R;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.R || language == Language.REBOL) {
            return true;
        } else {
            return false;
        }
    }

}
