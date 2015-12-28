package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;
import java.util.List;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public interface Resolver {
    public Language resolve(SourceFile sourceFile) throws IOException;

    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException;

    public boolean canResolve(Language language);
}
