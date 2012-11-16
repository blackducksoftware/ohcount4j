package net.ohloh.ohcount4j;

import java.io.IOException;
import java.util.List;

import net.ohloh.ohcount4j.detect.Detector;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineHandler;

public class FileCounter implements LineHandler {
    protected final SourceFile sourceFile;

    protected final List<String> filenames;

    protected Count count;

    public FileCounter(SourceFile sourceFile, List<String> filenames) {
        this.sourceFile = sourceFile;
        this.filenames = filenames;
    }

    public Count count() throws IOException {
        count = new Count();
        Language language = Detector.detect(sourceFile, filenames);
        if (language != null) {
            language.makeScanner().scan(sourceFile, this);
        }
        // Increment file count for each language seen
        for (LanguageCount c : count.getLanguageCounts()) {
            c.incrementFileCount();
        }
        // Increment grand total file count if we found something in this file
        if (count.getLanguageCounts().size() > 0) {
            count.incrementFileCount();
        }
        return count;
    }

    @Override
    public void handleLine(Line line) {
        count.add(line);
    }
}
