package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

public class ExtnINResolver extends AbstractExtnResolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        SourceFile stripped;
        String strippedPath = strippedPath(sourceFile.getPath());
        if (sourceFile.isContentsFromFile()) {
            stripped = new SourceFile(strippedPath, sourceFile.getReader());
        } else {
            stripped = new SourceFile(strippedPath, new String(sourceFile.getContents()));
        }
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
