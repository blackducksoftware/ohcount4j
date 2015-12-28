package com.blackducksoftware.ohcount4j.scan;

import java.io.IOException;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class BinaryScanner implements Scanner {

    private Language defaultLanguage = Language.BINARY;

    @Override
    public void setDefaultLanguage(Language language) {
        defaultLanguage = language;
    }

    @Override
    public Language getDefaultLanguage() {
        return defaultLanguage;
    }

    // Scan methods do nothing for binary files

    @Override
    public void scan(SourceFile blob, LineHandler handler) throws IOException {
    }

    @Override
    public void scan(char[] text, LineHandler handler) {
    }

    @Override
    public void scan(String text, LineHandler handler) {
    }

}
