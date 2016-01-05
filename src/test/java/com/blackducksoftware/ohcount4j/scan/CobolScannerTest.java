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

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class CobolScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.COBOL, new Line(Language.COBOL, BLANK), "\n");
        assertLine(Language.COBOL, new Line(Language.COBOL, BLANK), "     \n");
        assertLine(Language.COBOL, new Line(Language.COBOL, BLANK), "\t\n");
        assertLine(Language.COBOL, new Line(Language.COBOL, CODE), "PERFORM UNTIL NOT ValidCharacter\n");
        assertLine(Language.COBOL, new Line(Language.COBOL, COMMENT), "* Line comment\n");
        assertLine(Language.COBOL, new Line(Language.COBOL, COMMENT), "*\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.COBOL, new Line(Language.COBOL, BLANK), "     ");
        assertLine(Language.COBOL, new Line(Language.COBOL, BLANK), "\t");
        assertLine(Language.COBOL, new Line(Language.COBOL, CODE), "PERFORM UNTIL NOT ValidCharacter");
        assertLine(Language.COBOL, new Line(Language.COBOL, COMMENT), "* Line comment");
        assertLine(Language.COBOL, new Line(Language.COBOL, COMMENT), "*");
    }

    @Test
    public void sampleTest() {
        String code = "* Sample Program Written in Cobol\n"
                + "\n"
                + "PROCEDURE DIVISION.\n"
                + "Begin.\n"
                + "    DISPLAY \"Enter lower case character or digit. No data ends.\".\n"
                + "    ACCEPT Char.\n"
                + "    PERFORM UNTIL NOT ValidCharacter\n"
                + "        EVALUATE TRUE\n"
                + "           WHEN Vowel DISPLAY \"The letter \" Char \" is a vowel.\"\n"
                + "           WHEN Consonant DISPLAY \"The letter \" Char \" is a consonant.\"\n"
                + "           WHEN Digit DISPLAY Char \" is a digit.\"\n"
                + "           WHEN OTHER DISPLAY \"problems found\"\n"
                + "        END-EVALUATE\n"
                + "    END-PERFORM\n"
                + "    STOP RUN.;\n";

        Line[] expected = {
                new Line(Language.COBOL, COMMENT),
                new Line(Language.COBOL, BLANK),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
        };
        assertLines(Language.COBOL, expected, code);
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("cobol-1.cbl")));
        Line[] expected = {
                new Line(Language.COBOL, COMMENT),
                new Line(Language.COBOL, BLANK),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, CODE),
                new Line(Language.COBOL, BLANK)
        };
        assertLines(Language.COBOL, expected, sourceFile);
    }

}
