/**
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

/*
 * Copyright (C) 2016 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package com.blackducksoftware.ohcount4j;

import java.io.IOException;
import java.util.List;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import com.google.common.collect.Lists;

/**
 * @author mpujari
 *
 */
public class DiffSourceFileTest {

    @Test(dataProvider = "data")
    public void testData(String fromFileName, String toFileName, Language language, String fromFileContent,
            String toFileContent, int codeLinesAdded, int codeLinesRemoved, int commentLinesAdded, int commentLinesRemoved,
            int blankLinesAdded, int blankLinesRemoved) throws IOException {
        List<LanguageDiff> expectedDiff = Lists.newArrayList();
        LanguageDiff languageDiff = null;

        if (language != null) {
            languageDiff = new LanguageDiff(language, codeLinesAdded, codeLinesRemoved, commentLinesAdded,
                    commentLinesRemoved, blankLinesAdded, blankLinesRemoved);
            expectedDiff.add(languageDiff);
        }

        SourceFile fromSourceFile = (fromFileName != null) ? new SourceFile(fromFileName, fromFileContent) : null;
        SourceFile toSourceFile = (toFileName != null) ? new SourceFile(toFileName, toFileContent) : null;
        DiffSourceFile diffSrcFile = new DiffSourceFile();
        List<LanguageDiff> diff = diffSrcFile.diff(fromSourceFile, toSourceFile);
        Assert.assertEquals(diff, expectedDiff);
    }

    @DataProvider
    public Object[][] data() {
        return new Object[][] {
                { null, null, null, null, null, 0, 0, 0, 0, 0, 0 },
                { "test1.py", null, Language.PYTHON, " ", null, 0, 0, 0, 0, 0, 1 },
                { null, "test2.py", Language.PYTHON, null, " ", 0, 0, 0, 0, 1, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "", " ", 0, 0, 0, 0, 1, 0 },
                { "test1.py", "test2.py", Language.PYTHON, " ", "", 0, 0, 0, 0, 0, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "", "# c1", 0, 0, 1, 0, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "# c2", "", 0, 0, 0, 1, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\n", "#!/usr/bin/python\ni=i+1", 1, 0, 0, 0, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\ni=i+1", "#!/usr/bin/python\n", 0, 1, 0, 0, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\nj=j+1", "#!/usr/bin/python\ni=i+1", 1, 1, 0, 0, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\n\nj=j+1", "#!/usr/bin/python\ni=i+1", 1, 1, 0, 0, 0, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\nj=j+1", "#!/usr/bin/python\n\ni=i+1", 1, 1, 0, 0, 1, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\nj=j+1\n ", "#!/usr/bin/python\ni=i+1\n\n", 1, 1, 0, 0, 1, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\nj=j+1\n \n# c1", "#!/usr/bin/python\ni=i+1\n\n# c2", 1, 1, 1, 1, 1, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "j=j+1\n# c1\n ", null, 0, 1, 0, 1, 0, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\nj=j+1\n \n# c1", "#!/usr/bin/python\ni=i+1\n\n# c2\n ", 1, 1, 1, 1, 2, 1 },
                { "test1.py", "test2.py", Language.PYTHON, "#!/usr/bin/python\ni=i+1\n\n# c2\n ", "#!/usr/bin/python\nj=j+1\n \n# c1", 1, 1, 1, 1, 1, 2 },
                { "test1.py", "test2.py", Language.PYTHON, "j=j+1 # c1 \n", "j=j+1 # c1 \n i++", 1, 0, 0, 0, 0, 0 },
                { "test1.py", "test2.py", Language.PYTHON, "j=j+1 # c1 \n i++", "j=j+1 # c1 \n", 0, 1, 0, 0, 0, 0 },
        };
    }
}
