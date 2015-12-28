package com.blackducksoftware.ohcount4j;

import com.blackducksoftware.ohcount4j.scan.Line;
import com.blackducksoftware.ohcount4j.scan.LineHandler;

public class AnnotationWriter implements LineHandler {

    @Override
    public void handleLine(Line line) {
        System.out.print(line.toString());
    }

}
