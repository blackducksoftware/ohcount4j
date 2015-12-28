/*
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.SWIFT;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class SwiftScannerTeset extends AbstractBaseScannerTest {

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
