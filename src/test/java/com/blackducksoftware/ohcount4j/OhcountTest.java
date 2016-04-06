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
import java.util.ArrayList;
import java.util.List;

import org.testng.Assert;
import org.testng.annotations.Test;

public class OhcountTest extends AbstractOhcount4jTest {

	@Test
	public void testSummary() throws Exception {
		String sourceCodePath = getSourceCodePath("cStyle-issue-18");
		FileFinder ff = new FileFinder();
		ff.addPath(sourceCodePath);
		ArrayList<File> files = ff.getFiles();
		System.out.println(files);
		Count count = new ThreadedFileListCounter(4).count(files, getFilenames(files));

		LanguageCount cLang = getLanguageCountByLang(count, Language.C);
		Assert.assertNotNull(cLang);
		Assert.assertEquals(cLang.getFileCount(), 2); // c and h
		Assert.assertEquals(cLang.getCode(), 12);
		Assert.assertEquals(cLang.getComment(), 3);
		Assert.assertEquals(cLang.getBlank(), 5);
		Assert.assertEquals(cLang.getTotal(), 20);
	}

	private LanguageCount getLanguageCountByLang(Count count, Language language) {
		for (LanguageCount lc : count.getLanguageCounts()) {
			if (lc.language == language) {
				return lc;
			}
		}
		return null;
	}

	private List<String> getFilenames(List<File> files) {
		ArrayList<String> result = new ArrayList<String>();
		for (File file : files) {
			result.add(file.getPath());
		}
		return result;
	}

}
