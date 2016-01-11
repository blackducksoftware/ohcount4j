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

public class TclScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\n");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "     \n");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\t\n");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index)\n");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "# Line comment\n");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "#\n");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index) # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "     ");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\t");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index)");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "# Line comment");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "#");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index) # with comment");
    }

    @Test
    public void sampleTest() {
        String code = "@x = (1, 2, 3);      # Create a 3 element list/array\n"
                + "\t\n"
                + "foreach $i (@x) {\n"
                + "    print $i++ . \"-\";\n"
                + "}\n"
                + "# At this point, array contains 2, 3, 4!\n";

        Line[] expected = {
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, BLANK),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, COMMENT)
        };
        assertLines(Language.TCL, expected, code);
    }

}
