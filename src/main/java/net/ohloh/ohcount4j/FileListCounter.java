package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class FileListCounter {
    protected Count count = new Count();

    public Count count(List<File> files, List<String> filenames) throws IOException {
        for (File file : files) {
            Count c = new FileCounter(new SourceFile(file), filenames).count();
            count.add(c);
        }
        return count;
    }

    public Count count() {
        return count;
    }
}
