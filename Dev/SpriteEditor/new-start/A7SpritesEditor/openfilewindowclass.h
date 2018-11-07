#ifndef OPENFILEWINDOWCLASS_H
#define OPENFILEWINDOWCLASS_H

#include <QWidget>

namespace Ui {
class OpenFileWindowClass;
}

class OpenFileWindowClass : public QWidget
{
    Q_OBJECT

public:
    explicit OpenFileWindowClass(QWidget *parent = nullptr);
    ~OpenFileWindowClass();

private:
    Ui::OpenFileWindowClass *ui;
};

#endif // OPENFILEWINDOWCLASS_H
