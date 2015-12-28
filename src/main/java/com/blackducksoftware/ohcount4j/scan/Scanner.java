package com.blackducksoftware.ohcount4j.scan;

import java.io.IOException;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public interface Scanner {
    public void setDefaultLanguage(Language language);

    public Language getDefaultLanguage();

    public void scan(SourceFile blob, LineHandler handler) throws IOException;

    public void scan(char[] text, LineHandler handler);

    public void scan(String text, LineHandler handler);
}
