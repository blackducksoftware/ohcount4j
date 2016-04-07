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

import static java.io.File.separator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.testng.Assert;
import org.testng.annotations.Test;

public class FileFinderTest {

	@Test
	public void testFileFinderDefaultConstructor() throws IOException {
		FileFinder fileFinder = new FileFinder();
		String symlinkDir = getSourceCodePath("issue-17-symlink");

		fileFinder.addPath(symlinkDir); // add the dir which contains symlink

		ArrayList<File> files = fileFinder.getFiles();
		Assert.assertEquals(files.size(), 1);
		Assert.assertEquals(files.get(0).getName(), "c1.c");
	}

	@Test
	public void testFileFinderDefaultBehaviour() throws IOException {
		FileFinder fileFinder = new FileFinder(false);
		String symlinkDir = getSourceCodePath("issue-17-symlink");

		fileFinder.addPath(symlinkDir); // add the dir which contains symlink

		ArrayList<File> files = fileFinder.getFiles();
		Assert.assertEquals(files.size(), 1);
		Assert.assertEquals(files.get(0).getName(), "c1.c");
	}

	@Test
	public void testFileFinderWithSymbolicInclude() throws IOException {
		FileFinder fileFinder = new FileFinder(true);
		String symlinkDir = getSourceCodePath("issue-17-symlink");

		fileFinder.addPath(symlinkDir); // add the dir which contains symlink

		ArrayList<File> files = fileFinder.getFiles();
		Assert.assertEquals(files.size(), 2);
		Assert.assertEquals(files.get(0).getName(), "c1.c");
		Assert.assertEquals(files.get(1).getName(), "c1.c");
	}

	// TODO: Below method is code duplication, once PR #25 is merged in master
	// we can remove this by extending FileFinderTest with AbstractOhcount4jTest
	private String getSourceCodePath(String fileName) {
		StringBuilder srcPath = new StringBuilder(System.getProperty("user.dir"));
		srcPath.append(separator).append("src").append(separator).append("test").append(separator).append("src-code")
				.append(separator).append(fileName);
		return srcPath.toString();
	}

}
