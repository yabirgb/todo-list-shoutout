package io.todo

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication(scanBasePackageClasses = [Application::class])
class Application

fun main(args: Array<String>) {
	runApplication<Application>(*args)
}
