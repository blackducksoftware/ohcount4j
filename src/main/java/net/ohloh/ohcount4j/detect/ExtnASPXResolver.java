package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnASPXResolver implements Resolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if ("VB".equalsIgnoreCase(pageLanguage(sourceFile))) {
            return Language.ASPX_VB;
        } else {
            return Language.ASPX_CSHARP;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.ASPX_CSHARP ||
                language == Language.ASPX_VB) {
            return true;
        } else {
            return false;
        }
    }

    private static Pattern aspxPattern = Pattern.compile(
            "<%@.+\\bPage\\b.+\\bLanguage\\b\\s*=\\s*\"([^\"]+)\"",
            Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    private String pageLanguage(SourceFile sourceFile) throws IOException {
        Matcher m = aspxPattern.matcher(sourceFile.head(100));
        if (m.find()) {
            return m.group(1);
        } else {
            return null;
        }
    }
}
