package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.DirectoryWalker;

public class FileFinder extends DirectoryWalker<File> {
	protected ArrayList<File> results;

	public FileFinder() {
		this.results = new ArrayList<File>();
	}

	public ArrayList<File> getFiles() {
		return this.results;
	}

	public void addPath(String path) throws IOException {
		File f = new File(path);
		if (f.isDirectory()) {
			this.walk(f, (Collection<File>) results);
		} else {
			results.add(f);
		}
	}

	@Override
	protected void handleFile(File file, int depth, Collection<File> results) throws IOException {
		results.add(file);
	}
}