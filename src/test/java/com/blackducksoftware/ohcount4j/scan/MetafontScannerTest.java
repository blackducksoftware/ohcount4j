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
package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class MetafontScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.METAFONT, new Line(Language.METAFONT, BLANK), "\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, BLANK), "     \n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, BLANK), "\t\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "string base_name, base_version;\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "message \"Preloading the plain base\"\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, COMMENT), "% Line comment\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "def downto = step -1 until enddef;\n");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "def ... = .. tension atleast 1 .. enddef;\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.METAFONT, new Line(Language.METAFONT, BLANK), "     ");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, BLANK), "\t");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "string base_name, base_version;");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "message \"Preloading the plain base\"");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "void main(){");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, COMMENT), "% Line comment");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "def downto = step -1 until enddef;");
        assertLine(Language.METAFONT, new Line(Language.METAFONT, CODE), "def ... = .. tension atleast 1 .. enddef;");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("metafont.mf")));
        Line[] expected = {
                new Line(Language.METAFONT, COMMENT),
                new Line(Language.METAFONT, BLANK),
                new Line(Language.METAFONT, COMMENT),
                new Line(Language.METAFONT, COMMENT),
                new Line(Language.METAFONT, COMMENT),
                new Line(Language.METAFONT, BLANK),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, BLANK),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, BLANK),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, CODE),
                new Line(Language.METAFONT, BLANK),
                new Line(Language.METAFONT, CODE)
        };
        assertLines(Language.METAFONT, expected, sourceFile);
    }
}
