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

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class BooScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.BOO, new Line(Language.BOO, BLANK), "\n");
        assertLine(Language.BOO, new Line(Language.BOO, BLANK), "     \n");
        assertLine(Language.BOO, new Line(Language.BOO, BLANK), "\t\n");
        assertLine(Language.BOO, new Line(Language.BOO, CODE), "break unless i < 10 and i > 5\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* Block Comment */\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* /* Nested Block Comment*/ Test */\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "// Line comment\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "//\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "# Line comment\n");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "#\n");
        assertLine(Language.BOO, new Line(Language.BOO, CODE), "print((0, 'alpha', 4.5, char('d'))) # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.BOO, new Line(Language.BOO, BLANK), "     ");
        assertLine(Language.BOO, new Line(Language.BOO, BLANK), "\t");
        assertLine(Language.BOO, new Line(Language.BOO, CODE), "break unless i < 10 and i > 5");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* Block Comment */");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* /* Nested Block Comment*/ Test */");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "// Line comment");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "//");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "# Line comment");
        assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "#");
        assertLine(Language.BOO, new Line(Language.BOO, CODE), "print((0, 'alpha', 4.5, char('d'))) // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "# Sample Program Written in Boo\n"
                + "/* Code For Simple Use\n"
                + "\n"
                + "		of an array /*\n"
                + "			This comment has a nested /*\n"
                + "			Comment inside of it */ */\n"
                + "*/\n"
                + "print((0, 'alpha', 4.5, char('d')))\n"
                + "print array('abcdefghij') // Print the array\n"
                + "l = array(range(5))\n"
                + "print l\n"
                + "l[2] = 5\n"
                + "print l\n";

        Line[] expected = {
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, BLANK),
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, CODE),
                new Line(Language.BOO, CODE),
                new Line(Language.BOO, CODE),
                new Line(Language.BOO, CODE),
                new Line(Language.BOO, CODE),
                new Line(Language.BOO, CODE)
        };
        assertLines(Language.BOO, expected, code);
    }

    @Test
    public void unterminatedNestedCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/* /*  */\n\n\n";

        Line[] expected = {
                new Line(Language.BOO, COMMENT),
                new Line(Language.BOO, BLANK),
                new Line(Language.BOO, BLANK)
        };
        assertLines(Language.BOO, expected, code);
    }
}
