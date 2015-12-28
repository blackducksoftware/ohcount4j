package net.ohloh.ohcount4j.detect;

import static java.util.regex.Pattern.CASE_INSENSITIVE;
import static java.util.regex.Pattern.DOTALL;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnASPXResolver extends AbstractExtnResolver {

    private static final String VB_STR = "VB";

    private static final Pattern ASPX_PATTERN = Pattern.compile(
            "<%@.+\\bPage\\b.+\\bLanguage\\b\\s*=\\s*\"([^\"]+)\"", DOTALL | CASE_INSENSITIVE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (VB_STR.equalsIgnoreCase(pageLanguage(sourceFile))) {
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
        if (language == Language.ASPX_CSHARP || language == Language.ASPX_VB) {
            return true;
        } else {
            return false;
        }
    }

    private String pageLanguage(SourceFile sourceFile) throws IOException {
        Matcher m = ASPX_PATTERN.matcher(headContent(sourceFile));
        if (m.find()) {
            return m.group(1);
        } else {
            return null;
        }
    }

}
