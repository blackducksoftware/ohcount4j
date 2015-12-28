package com.blackducksoftware.ohcount4j.detect;

import static com.blackducksoftware.ohcount4j.Language.CLASSIC_BASIC;
import static com.blackducksoftware.ohcount4j.Language.LIMBO;
import static com.blackducksoftware.ohcount4j.Language.STRUCTURED_BASIC;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnBResolver extends AbstractExtnResolver {

    // /(implement[ \t])|(include[ \t]+"[^"]*";)|
    // ((return|break|continue).*;|(pick|case).*\{)/
    private static final Pattern LIMBO_PATTERN = Pattern.compile(
            "^\\s*implement\\s+\\S+;|" +
                    "^\\s*include\\s+\"[^\"]*\"\\s*;|" +
                    "\\b(return|break|continue)\\b.*;|" +
                    "\\b(pick|case)\\b.*\\{"
            );

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (LIMBO_PATTERN.matcher(getCharSequence(sourceFile)).find()) {
            return Language.LIMBO;
        } else {
            return new ExtnBASResolver().resolve(sourceFile, filenames);
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == CLASSIC_BASIC || language == STRUCTURED_BASIC || language == LIMBO) {
            return true;
        } else {
            return false;
        }
    }

}
