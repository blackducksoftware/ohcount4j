package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

/*
 * Distinguishes between 6502 Assembly and Advanced Stream Redirector XML files
 */
public class ExtnASXResolver implements Resolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (xmlPattern.matcher(sourceFile.getCharSequence()).find()) {
            return Language.XML;
        } else {
            return Language.ASSEMBLY;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.ASSEMBLY ||
                language == Language.XML) {
            return true;
        } else {
            return false;
        }
    }

    private static Pattern xmlPattern = Pattern.compile(
            "^\\s*<asx", Pattern.CASE_INSENSITIVE);
}
