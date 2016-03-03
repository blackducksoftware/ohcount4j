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

public class ColdFusionScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, BLANK), "\n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, BLANK), "     \n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, BLANK), "\t\n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, CODE), "<cfset today = DateFormat(now(), \"dddd, mmmm d, yyyy\")>\n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, COMMENT), "<!--- Block Comment --->\n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, COMMENT), "<!--- <!--- Nested Block Comment ---> Test --->\n");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, CODE),
                "<cfset today = DateFormat(now(), \"dddd, mmmm d, yyyy\")> <!--- with comment --->\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, BLANK), "     ");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, BLANK), "\t");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, CODE), "<cfset today = DateFormat(now(), \"dddd, mmmm d, yyyy\")>");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, COMMENT), "<!--- Block Comment --->");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, COMMENT), "<!--- <!--- Nested Block Comment ---> Test --->");
        assertLine(Language.COLDFUSION, new Line(Language.COLDFUSION, CODE),
                "<cfset today = DateFormat(now(), \"dddd, mmmm d, yyyy\")> <!--- with comment --->");
    }

    @Test
    public void sampleTest() {
        String code = "<!--- Sample Program\n"
                + "		Written in Cold Fusion\n"
                + "		<!--- Nested Comments\n"
                + "			supported --->\n"
                + "--->\n"
                + "<cfoutput>\n"
                + "<cfset today =\n"
                + "DateFormat(now(), \"dddd, mmmm d, yyyy\")>\n"
                + "<i>#today#</i>\n"
                + "</cfoutput>\n";

        Line[] expected = {
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, CODE),
                new Line(Language.COLDFUSION, CODE),
                new Line(Language.COLDFUSION, CODE),
                new Line(Language.COLDFUSION, CODE),
                new Line(Language.COLDFUSION, CODE)
        };
        assertLines(Language.COLDFUSION, expected, code);
    }

    @Test
    public void unterminatedNestedCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "<!--- <!---  --->\n\n\n";

        Line[] expected = {
                new Line(Language.COLDFUSION, COMMENT),
                new Line(Language.COLDFUSION, BLANK),
                new Line(Language.COLDFUSION, BLANK)
        };
        assertLines(Language.COLDFUSION, expected, code);
    }
}
