package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnINResolver implements Resolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        SourceFile stripped = new SourceFile(strippedPath(sourceFile.getPath()), sourceFile.getReader());
        return Detector.detect(stripped);
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        return true;
    }

    public String strippedPath(String path) {
        if (path.endsWith(".in")) {
            return path.substring(0, path.length() - 3);
        } else {
            return path;
        }
    }
}
