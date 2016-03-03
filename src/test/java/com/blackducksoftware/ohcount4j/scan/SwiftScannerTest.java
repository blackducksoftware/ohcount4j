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
import static com.blackducksoftware.ohcount4j.Language.SWIFT;

import org.testng.annotations.Test;

public class SwiftScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(SWIFT, new Line(SWIFT, BLANK), "\n");
        assertLine(SWIFT, new Line(SWIFT, BLANK), "     \n");
        assertLine(SWIFT, new Line(SWIFT, BLANK), "\t\n");
        assertLine(SWIFT, new Line(SWIFT, CODE), "import UIKit\n");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/* Block Comment */\n");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/** Block Comment **/\n");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/*** Block Comment ***/\n");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "// Line comment\n");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "//\n");
        assertLine(SWIFT, new Line(SWIFT, CODE), "import UIKit // with comment\n");
        assertLine(SWIFT, new Line(SWIFT, CODE), "print(\"The status message is \\(http404Error.1)\")\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(SWIFT, new Line(SWIFT, BLANK), "     ");
        assertLine(SWIFT, new Line(SWIFT, BLANK), "\t");
        assertLine(SWIFT, new Line(SWIFT, CODE), "import UIKit");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/* Block Comment */");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/** Block Comment **/");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "/*** Block Comment ***/");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "// Line comment");
        assertLine(SWIFT, new Line(SWIFT, COMMENT), "//");
        assertLine(SWIFT, new Line(SWIFT, CODE), "import UIKit // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "import UIKit\n"
                + "\n"
                + "@UIApplicationMain\n"
                + "class AppDelegate: UIResponder, UIApplicationDelegate {\n"
                + "  var window: UIWindow?\n"
                + "\n"
                + "func applicationWillTerminate(application: UIApplication) {\n"
                + "// Called when the application is about to terminate\n"
                + "}\n"
                + "}";

        Line[] expected = {
                new Line(SWIFT, COMMENT),
                new Line(SWIFT, COMMENT),
                new Line(SWIFT, BLANK),
                new Line(SWIFT, CODE),
                new Line(SWIFT, BLANK),
                new Line(SWIFT, CODE),
                new Line(SWIFT, CODE),
                new Line(SWIFT, CODE),
                new Line(SWIFT, BLANK),
                new Line(SWIFT, CODE),
                new Line(SWIFT, COMMENT),
                new Line(SWIFT, CODE),
                new Line(SWIFT, CODE)
        };
        assertLines(SWIFT, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(SWIFT, CODE),
                new Line(SWIFT, CODE),
                new Line(SWIFT, BLANK)
        };
        assertLines(SWIFT, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(SWIFT, COMMENT),
                new Line(SWIFT, BLANK),
                new Line(SWIFT, BLANK)
        };
        assertLines(SWIFT, expected, code);
    }

}
