/*
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.DirectoryWalker;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.HiddenFileFilter;
import org.apache.commons.io.filefilter.IOFileFilter;

public class FileFinder extends DirectoryWalker<File> {

	protected ArrayList<File> results;

	private boolean processSymbolicLinks;

	private static final IOFileFilter dirFilter = HiddenFileFilter.VISIBLE;

	private static final IOFileFilter fileFilter = FileFilterUtils.and(HiddenFileFilter.VISIBLE,
			FileFilterUtils.sizeFileFilter(1000000, false));

	public FileFinder() {
		this(false); // by default we don't process symbolic links, they are
						// ignored.
	}

	public FileFinder(boolean processSymbolicLinks) {
		super(dirFilter, fileFilter, -1);
		this.processSymbolicLinks = processSymbolicLinks;
		results = new ArrayList<File>();
	}

	public ArrayList<File> getFiles() {
		return results;
	}

	public void addPath(String path) throws IOException {
		File file = new File(path);
		if (file.isDirectory()) {
			if (canAddDirectoryForProcessing(file)) {
				walk(file, results);
			}
		} else {
			results.add(file);
		}
	}

	@Override
	protected boolean handleDirectory(File directory, int depth, Collection<File> results) throws IOException {
		return canAddDirectoryForProcessing(directory);
	}

	private boolean canAddDirectoryForProcessing(File directory) {
		boolean isSymbolicLink = Files.isSymbolicLink(directory.toPath());
		if(isSymbolicLink) {
			return processSymbolicLinks;
		} else {
			return true;
		}
	}

	@Override
	protected void handleFile(File file, int depth, Collection<File> results) throws IOException {
		results.add(file);
	}

}
