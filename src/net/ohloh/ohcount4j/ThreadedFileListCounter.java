package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.List;

import net.ohloh.ohcount4j.Count;

public class ThreadedFileListCounter {
	protected Count count = new Count();

	public Count count(List<File> files, List<String> filenames) throws IOException {
		for (File file : files) {
			Count c = new FileCounter(new SourceFile(file), filenames).count();
			this.count.add(c);
		}
		return this.count;
	}

	public Count count() {
		return this.count;
	}
}