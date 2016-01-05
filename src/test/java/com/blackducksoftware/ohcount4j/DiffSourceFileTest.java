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

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class DiffSourceFileTest {

    @Test(dataProvider = "data")
    public void testData(String fromFileContent, String toFileContent, int codeLinesAdded,
            int codeLinesRemoved, int commentLinesAdded, int commentLinesRemoved,
            int blankLinesAdded, int blankLinesRemoved) throws IOException {
        Diff expectedDiff = new Diff(codeLinesAdded, codeLinesRemoved, commentLinesAdded,
                commentLinesRemoved, blankLinesAdded, blankLinesRemoved);
        SourceFile fromSourceFile = new SourceFile("test1.py", fromFileContent);
        SourceFile toSourceFile = new SourceFile("test2.py", toFileContent);
        DiffSourceFile diffSrcFile = new DiffSourceFile();
        Diff diff = diffSrcFile.diff(fromSourceFile, toSourceFile);
        Assert.assertEquals(diff, expectedDiff);
    }

    @DataProvider
    public Object[][] data() {
        return new Object[][] {
                { "", "", 0, 0, 0, 0, 0, 0 },
                { null, null, 0, 0, 0, 0, 0, 0 },
                { "", null, 0, 0, 0, 0, 0, 0 },
                { null, "", 0, 0, 0, 0, 0, 0 },
                { "", " ", 0, 0, 0, 0, 1, 0 },
                { " ", "", 0, 0, 0, 0, 0, 1 },
                { "", "# c1", 0, 0, 1, 0, 0, 0 },
                { "# c2", "", 0, 0, 0, 1, 0, 0 },
                { "#!/usr/bin/python\n", "#!/usr/bin/python\ni=i+1", 1, 0, 0, 0, 0, 0 },
                { "#!/usr/bin/python\ni=i+1", "#!/usr/bin/python\n", 0, 1, 0, 0, 0, 0 },
                { "#!/usr/bin/python\nj=j+1", "#!/usr/bin/python\ni=i+1", 1, 1, 0, 0, 0, 0 },
                { "#!/usr/bin/python\n\nj=j+1", "#!/usr/bin/python\ni=i+1", 1, 1, 0, 0, 0, 1 },
                { "#!/usr/bin/python\nj=j+1", "#!/usr/bin/python\n\ni=i+1", 1, 1, 0, 0, 1, 0 },
                { "#!/usr/bin/python\nj=j+1\n ", "#!/usr/bin/python\ni=i+1\n\n", 1, 1, 0, 0, 1, 1 },
                { "#!/usr/bin/python\nj=j+1\n \n# c1", "#!/usr/bin/python\ni=i+1\n\n# c2", 1, 1, 1, 1, 1, 1 },
                { "j=j+1\n# c1\n ", null, 0, 1, 0, 1, 0, 1 },
                { "#!/usr/bin/python\nj=j+1\n \n# c1", "#!/usr/bin/python\ni=i+1\n\n# c2\n ", 1, 1, 1, 1, 2, 1 },
                { "#!/usr/bin/python\ni=i+1\n\n# c2\n ", "#!/usr/bin/python\nj=j+1\n \n# c1", 1, 1, 1, 1, 1, 2 },
                { "j=j+1 # c1 \n", "j=j+1 # c1 \n i++", 1, 0, 0, 0, 0, 0 },
                { "j=j+1 # c1 \n i++", "j=j+1 # c1 \n", 0, 1, 0, 0, 0, 0 },
        };
    }
}
