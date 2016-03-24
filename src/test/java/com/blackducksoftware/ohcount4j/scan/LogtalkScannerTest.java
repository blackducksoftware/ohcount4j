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
import static com.blackducksoftware.ohcount4j.Language.LOGTALK;

import org.testng.annotations.Test;

public class LogtalkScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(LOGTALK, new Line(LOGTALK, BLANK), "\n");
        assertLine(LOGTALK, new Line(LOGTALK, BLANK), "     \n");
        assertLine(LOGTALK, new Line(LOGTALK, BLANK), "\t\n");
        assertLine(LOGTALK, new Line(LOGTALK, CODE), ":- public(new/4).\n");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "/* Block Comment */\n");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "% Line comment\n");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "%\n");
        assertLine(LOGTALK, new Line(LOGTALK, CODE), "new(Radius, X, Y, Circle) :-  % this would be a \"constructor\" in other languages\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(LOGTALK, new Line(LOGTALK, BLANK), "     ");
        assertLine(LOGTALK, new Line(LOGTALK, BLANK), "\t");
        assertLine(LOGTALK, new Line(LOGTALK, CODE), ":- end_object.");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "/* Block Comment */");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "% Line comment");
        assertLine(LOGTALK, new Line(LOGTALK, COMMENT), "%");
        assertLine(LOGTALK, new Line(LOGTALK, CODE), "new(Radius, X, Y, Circle) :-  % this would be a \"constructor\" in other languages\n");
    }

    @Test
    public void sampleTest() {
        String code = ":- object(c42,         % a static instance of \"circle\"; of course, you\n"
                + "    instantiates(circle)).  % can also create dynamic instances at runtime\n"
                + "               % by sending the new/4 message to \"circle\"\n"
                + "\n"
                + "   position(3.7, 4.5).\n"
                + "  radius(2.8).\n"
                + " /*\n"
                + "        comment1\n"
                + "         comment2 */\n"
                + "\n"
                + ":- end_object.";

        Line[] expected = {
                new Line(LOGTALK, CODE),
                new Line(LOGTALK, CODE),
                new Line(LOGTALK, COMMENT),
                new Line(LOGTALK, BLANK),
                new Line(LOGTALK, CODE),
                new Line(LOGTALK, CODE),
                new Line(LOGTALK, COMMENT),
                new Line(LOGTALK, COMMENT),
                new Line(LOGTALK, COMMENT),
                new Line(LOGTALK, BLANK),
                new Line(LOGTALK, CODE),
        };
        assertLines(LOGTALK, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(LOGTALK, COMMENT),
                new Line(LOGTALK, BLANK),
                new Line(LOGTALK, BLANK)
        };
        assertLines(LOGTALK, expected, code);
    }

}
