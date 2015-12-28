package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPPResolver extends AbstractExtnResolver {

    private static final Pattern PUPPET_PATTERN = Pattern.compile(
            "(" +
                    "\\b(enable|ensure|content|source)\\s*=>|" +
                    "\\binclude\\s+\\w+\\b|" +
                    "\\bdefine\\s+\\w+\\s*\\(|" +
                    "\\bclass\\s+\\w+\\s*\\{" +
                    ")", Pattern.MULTILINE);

    private static final Pattern PASCAL_PATTERN = Pattern.compile(
            "\\bend\\.|\\{\\s*\\$i(nclude)?\\s+", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        // Both Pascal and Puppet have an 'include' variant, but Pascal's
        // is more strict. Thus we check for Pascal keywords first, then Puppet.
        if (PASCAL_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.PASCAL;
        } else if (PUPPET_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.PUPPET;
        } else {
            return Language.PASCAL;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.PASCAL || language == Language.PUPPET) {
            return true;
        } else {
            return false;
        }
    }

}
