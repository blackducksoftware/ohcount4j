package com.blackducksoftware.ohcount4j.detect;

import static java.util.regex.Pattern.CASE_INSENSITIVE;
import static java.util.regex.Pattern.MULTILINE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPLResolver extends AbstractExtnResolver {

    // perlShebangPattern
    private static final Pattern PERL_SHEBANG_PATTERN = Pattern.compile("^\\#\\!.*\\bperl\\b", CASE_INSENSITIVE);

    private static final Pattern PROLOG_RULE_PATTERN = Pattern.compile("\\:\\-\\s+", MULTILINE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (PERL_SHEBANG_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.PERL;
        }
        if (PROLOG_RULE_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
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
        if (language == Language.PERL || language == Language.PROLOG) {
            return true;
        } else {
            return false;
        }
    }

}
