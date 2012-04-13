package net.ohloh.ohcount4j;

import java.io.IOException;
import java.util.List;

import net.ohloh.ohcount4j.Count;
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
		this.count = new Count();
		Language language = Detector.detect(sourceFile, filenames);
		if (language != null) {
			language.makeScanner().scan(sourceFile, this);
		}
		// Increment file count for each language seen
		for (LanguageCount c : this.count.getLanguageCounts()) {
			c.incrementFileCount();
		}
		// Increment grand total file count if we found something in this file
		if (this.count.getLanguageCounts().size() > 0) {
			this.count.incrementFileCount();
		}
		return this.count;
	}

	@Override
	public void handleLine(Line line) {
		count.add(line);
	}
}