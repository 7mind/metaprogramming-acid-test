plugins {
    kotlin("jvm") version "2.0.21"
    application
}

group = "io.functoid"
version = "1.0.0"

repositories {
    mavenCentral()
}

dependencies {
    // Kotlin stdlib
    implementation(kotlin("stdlib"))
    implementation(kotlin("reflect"))

    // Testing
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.1")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(21)
}

sourceSets {
    main {
        kotlin.srcDir("src/main/kotlin")
    }
    test {
        kotlin.srcDir("src/test/kotlin")
    }
}

application {
    mainClass.set("io.functoid.examples.GenericTypesDemo")
}
