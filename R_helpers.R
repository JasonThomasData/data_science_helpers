#install.packages("nortest")
library(nortest)

get_linear_intersection = function(y_int_1, gradient_1, y_int_2=NULL, gradient_2=NULL) {
    # This is expected to work for linear functions only. Quadratics etc are not gauranteed to have an x intercept
    if (is.null(vertical) && (is.null(y_int_2) || is.null(gradient_2))) {
        stop("Given y_int_2 and gradient_2, vertical must be NULL")
    }
    if (!is.null(vertical) && (!is.null(y_int_2) || !is.null(gradient_2))) {
        stop("Given vertical, then y_int_2 and gradient_2 must be NULL")
    }
    x = (y_int_2 - y_int_1) / (gradient_1 - gradient_2)
    y_1 = gradient_1 * x + y_int_1
    y_2 = gradient_2 * x + y_int_2
    if (round(y_1, 10) == round(y_2, 10)) {
        point = c(x, y_1)
        return (point)
    } else {
        print("No intersection") 
    }
}

feature_scale = function(original_data) {
    return ((original_data-min(original_data))/(max(original_data)-min(original_data)))
}

plot_correlation = function(col_1, col_2, col_1_label, col_2_label, cor_method="pearson") {
    correlation_summary = cor.test(col_1, col_2, method=cor_method)
    title = sprintf("Correlation coefficient: %f", correlation_summary$estimate) 
    jpeg(file = sprintf("%s_correlation.jpg", col_1_label))
    plot(col_1,
        col_2,
        abline( lm(col_2~col_1)),
        xlab=col_1_label,
        ylab=col_2_label,
        main=title)
    dev.off()
}

is_normal_distribution = function(data) {
    sw_test_results = shapiro.test(data)
    sw_p_value = sw_test_results[2]
    ad_test_results = ad.test(data)
    ad_p_value = ad_test_results[2]
    alpha_level = 0.05
    if (sw_p_value > alpha_level || ad_p_value > alpha_level) {
        print("TRUE")
        TRUE
    } else {
        print("NOT NORMAL")
        FALSE
    }
}

plot_distribution = function(data, label, distribution_is_normal) {
    jpeg(file = sprintf("%s_distribution.jpg", label))
    main_heading = sprintf("Histogram of %s", label)
    sub_heading = sprintf("Normal distribution: %s", distribution_is_normal)
    hist(data,
        xlab=label,
        main=main_heading,
        sub=sub_heading)
    dev.off()
}

summarise = function(data, row_header) {
    header = sprintf("%s, mean, SD, median, IQR", row_header)
    message(header)
    number_of_columns = ncol(data)
    if (is.null(number_of_columns) || number_of_columns == 1) {
        summary_row = sprintf("%s, %#.2f, %#.2f, %#.2f, %#.2f", ".", mean(data), sd(data), median(data), IQR(data))
        message(summary_row)
    } else {
        for (i in (1:number_of_columns)) {
            column = data[,i]
            summary_row = sprintf("%s, %#.2f, %#.2f, %#.2f, %#.2f", names(data)[i], mean(column), sd(column), median(column), IQR(column))
            message(summary_row)
        }
    }
}

plot_multiline = function(series_1, label_1, series_2, label_2, colour_1="red", colour_2="green", filename="multiline.jpg") {
    jpeg(file = filename)
    plot(series_1, ylim=c(0,1), type="l", col=colour_1, ylab="Y")
    lines(series_2, col=colour_2)
    legend("topleft", legend=c(label_1, label_2),
        col=c(colour_1, colour_2), lty=1:1, cex=0.8)
    dev.off()
}