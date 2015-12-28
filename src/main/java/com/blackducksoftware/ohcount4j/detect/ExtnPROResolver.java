package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPROResolver extends AbstractExtnResolver {

    private static final Pattern QMAKE_PATTERN = Pattern.compile("\\b(SOURCES|CONFIG)\\s*\\+\\=");

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (QMAKE_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.MAKE; // Actually QMAKE. Should this be a distinct language?
        } else {
            return Language.PVWAVE;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.MAKE || language == Language.PVWAVE) {
            return true;
        } else {
            return false;
        }
    }

}
