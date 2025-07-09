# COBOL Learning Guide

![COBOL Logo](https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Cobol_logo.svg/1200px-Cobol_logo.svg.png)

A comprehensive guide to getting started with COBOL programming, including installation, basic syntax, and practical examples.

## Table of Contents
1. [Installation](#installation)
2. [Running Programs](#running-programs)
3. [Basic Syntax](#basic-syntax)
4. [Data Types](#data-types)
5. [Code Structure](#code-structure)
6. [Example Program](#example-program)
7. [Resources](#resources)

## Installation <a name="installation"></a>

### macOS/Linux
```bash
# Install Homebrew (if not installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Add Homebrew to PATH
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zshrc
source ~/.zshrc

# Install GNU COBOL
brew install gnu-cobol

# Verify installation
cobc --version
