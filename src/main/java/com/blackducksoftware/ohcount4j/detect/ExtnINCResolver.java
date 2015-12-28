package com.blackducksoftware.ohcount4j.detect;

import static java.util.regex.Pattern.MULTILINE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnINCResolver extends AbstractExtnResolver {

    private static final Pattern PHP_PATTERN = Pattern.compile("^\\s*<\\?php", MULTILINE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        Matcher m = PHP_PATTERN.matcher(getCharSequence(sourceFile));
        if (m.find()) {
            return Language.PHP;
        } else {
            return Language.BINARY;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.BINARY || language == Language.PHP) {
            return true;
        } else {
            return false;
        }
    }

}
