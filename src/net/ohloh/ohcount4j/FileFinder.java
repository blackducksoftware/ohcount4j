package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.DirectoryWalker;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.HiddenFileFilter;
import org.apache.commons.io.filefilter.IOFileFilter;

public class FileFinder extends DirectoryWalker<File> {
	protected ArrayList<File> results;

	private static final IOFileFilter dirFilter = HiddenFileFilter.VISIBLE;

	private static final IOFileFilter fileFilter =
			FileFilterUtils.and(HiddenFileFilter.VISIBLE,
								FileFilterUtils.sizeFileFilter(1000000, false));

	public FileFinder() {
		super(dirFilter, fileFilter, -1);
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