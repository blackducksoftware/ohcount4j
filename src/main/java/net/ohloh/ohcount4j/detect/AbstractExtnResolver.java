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
package net.ohloh.ohcount4j.detect;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;
import net.ohloh.ohcount4j.SourceFileUtils;

/**
 * @author mpujari
 *
 */
abstract class AbstractExtnResolver implements Resolver {

    protected String headContent(SourceFile sourceFile) throws IOException {
        return SourceFileUtils.head(sourceFile);
    }

    protected CharSequence getCharSequence(SourceFile sourceFile) throws IOException {
        return sourceFile.getCharSequence();
    }

    protected char[] getContents(SourceFile sourceFile) throws IOException {
        return SourceFileUtils.getContents(sourceFile);
    }

}
