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

public class SmalltalkScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "     \n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\t\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), "^Student new name: aPerson name\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"\"\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), " y := y + 7. // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "     ");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\t");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), "^Student new name: aPerson name");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"\"");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), " y := y + 7. // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "\"Simple piece of a Smalltalk\n"
                + "			Program\n"
                + "\n"
                + "\"\n"
                + "name: aName address: adAddress\n"
                + "\"Set the receiver's name and address\n"
                + "to the specified values.\"\n"
                + "self name: aName.\n"
                + "self address: anAddress\n";

        Line[] expected = {
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, BLANK),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, CODE),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, CODE),
                new Line(Language.SMALLTALK, CODE)
        };
        assertLines(Language.SMALLTALK, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "\"\n\n\n";

        Line[] expected = {
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, BLANK),
                new Line(Language.SMALLTALK, BLANK)
        };
        assertLines(Language.SMALLTALK, expected, code);
    }
}
