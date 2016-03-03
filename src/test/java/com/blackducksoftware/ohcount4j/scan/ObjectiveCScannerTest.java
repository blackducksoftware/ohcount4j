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

public class ObjectiveCScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, BLANK), "\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, BLANK), "     \n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, BLANK), "\t\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, CODE), "NSMutableArray *myArray = nil;\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "/* Block Comment */\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "// Line comment\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "//\n");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, CODE), "NSMutableArray *myArray = nil;  // nil is essentially the same as NULL\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, BLANK), "     ");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, BLANK), "\t");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, CODE), "NSMutableArray *myArray = nil;");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "/* Block Comment */");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "// Line comment");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, COMMENT), "//");
        assertLine(Language.OBJECTIVE_C, new Line(Language.OBJECTIVE_C, CODE), "NSMutableArray *myArray = nil;  // nil is essentially the same as NULL");
    }

    @Test
    public void helloWorld() {
        String code = "/* Objective C \"Hello World\"\n"
                + "Test Program\n"
                + "\n"
                + "*/\n"
                + "#import <Foundation/Foundation.h>\n"
                + "\n"
                + "int main (int argc, const char * argv[])\n"
                + "{\n"
                + "		NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];\n"
                + "	    NSLog (@\"Hello, World!\");\n"
                + "	    [pool drain];\n"
                + "	    return 0;\n"
                + "}\n"
                + "//";

        Line[] expected = {
                new Line(Language.OBJECTIVE_C, COMMENT),
                new Line(Language.OBJECTIVE_C, COMMENT),
                new Line(Language.OBJECTIVE_C, BLANK),
                new Line(Language.OBJECTIVE_C, COMMENT),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, BLANK),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, CODE),
                new Line(Language.OBJECTIVE_C, COMMENT)
        };
        assertLines(Language.OBJECTIVE_C, expected, code);
    }

}
