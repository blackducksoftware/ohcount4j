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

/**
 * @author gandhip
 *
 */
public class BlitzMaxScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     \n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\t\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "SuperStrict\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Import BRL.StandardIO\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, COMMENT), "' comment\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "print \"hello rem fish ' \"\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Type TABC \n");
    }

    @Test
    public void helloWorld() {
        String code = "Rem\n" // Multi-line comment
                + "bbdoc: docs\n"
                + "End Rem\n"
                + "\n"
                + "Import BRL.StandardIO\n"
                + "\n"
                + "\tMethod hello() \n"
                + "' single line comment \n" // single line comment
                + "\t\tEnd Method\n";
        Line[] expected = {
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
        };
        assertLines(Language.BLITZMAX, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     ");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     ");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\t");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "SuperStrict");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Import BRL.StandardIO");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, COMMENT), "' comment");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "print \"hello rem fish ' \"");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Type TABC ");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("blitzmax.bmx")));
        Line[] expected = {
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE)
        };
        assertLines(Language.BLITZMAX, expected, sourceFile);
    }

}
