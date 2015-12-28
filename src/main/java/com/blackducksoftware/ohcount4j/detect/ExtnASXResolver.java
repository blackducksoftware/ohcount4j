package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/*
 * Distinguishes between 6502 Assembly and Advanced Stream Redirector XML files
 */
public class ExtnASXResolver extends AbstractExtnResolver {

    private static final Pattern XML_PATTERN = Pattern.compile("^\\s*<asx", Pattern.CASE_INSENSITIVE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (XML_PATTERN.matcher(getCharSequence(sourceFile)).find()) {
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
        if (language == Language.ASSEMBLY || language == Language.XML) {
            return true;
        } else {
            return false;
        }
    }

}
