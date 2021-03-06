#!/bin/python3
#
USAGE = """Workhorse

USAGE:
    horse                             Print information about all projects
    horse <PROJECT NAME>              Print information about a project
    horse <PROJECT NAME> [Xh] [Ym]    Add X hours and/or Y minutes to project
    horse -h                          Print this help information"""

### CONFIGURATION - change these ###
CONFIG_LOCATION = '/home/rpj/.workhorse'

##############################################################################
import sys, os, math

tags     = []
projects = []

class Tag():
    def __init__(self, name):
        self.name = name

    def __str__(self):
        # tag_projs = []
        income = 0
        hours = 0
        minutes = 0

        for project in projects:
            if self.name in project.tags:
                hours += project.hours
                minutes += project.minutes
                income += project.income

        return ('%s\n\tTotal time: %dH%dM\n\tTotal income: %d\n' %
                (self.name, hours, minutes, income))

    def save_str(self):
        return '%s\n' % (self.name)

def load_tag(tag_name):
    tags.append(Tag(tag_name))

class Project():
    def __init__(self, name, h, m, tags, income):
        self.name    = name
        self.hours   = h
        self.minutes = m
        self.tags    = tags.split(',')
        self.income  = int(income)

        if self.tags[0] == '':
            self.tags = []

    def __str__(self):
        return '%s %dH %dM $%d' % (self.name, self.hours, self.minutes, self.income)

    def add(self, hours, minutes):
        self.minutes    += minutes
        hours           += math.floor(self.minutes / 60.0)
        self.minutes    = self.minutes % 60

        self.hours      += hours

    def save_str(self):
        return '%s:%dH:%dM:%s:%d\n' % (self.name, self.hours, self.minutes,
                ','.join(self.tags), self.income)

def load_project(parts):
    # [:-1] removes the M and H from e.g. 5H20M
    new = Project(parts[0], int(parts[1][:-1]), int(parts[2][:-1]), parts[3],
            parts[4])
    projects.append(new)

def load_file():
    f           = open(CONFIG_LOCATION, 'r')
    found_data  = False
    found_tags  = False

    for line in f:
        parts = line.split(':')

        if found_data:
            load_project(parts)
        elif found_tags:
            if '%% data' in parts[0]:
                found_data = True
                found_tags = False
            else:
                load_tag(parts[0].rstrip())
        elif '%% tags' in parts[0]:
            found_tags = True

    f.close()

def show_projects():
    for i in range(len(projects)):
        print('%d. %s' % (i+1, projects[i]))

def get_tag_from_arg(arg):
    for tag in tags:
        if tag.name.startswith(arg):
            return tag

    return None

def get_project_from_arg(arg):
    if arg.isnumeric():
        index = int(arg) - 1
        return projects[index]
    else:
        for project in projects:
            if project.name.startswith(arg):
                return project

    return None
    # raise Exception('no such project: "%s"' % (arg))

def show_project(arg):
    project = get_project_from_arg(arg)

    if project == None:
        tag = get_tag_from_arg(arg)

        if tag == None:
            print('no such project or tag')
            return

        print(tag)
        return

    print(project)

def add_hours(name, hours, minutes):
    project = get_project_from_arg(name)

    # new project
    if project == None:
        projects.append(Project(name, hours, minutes, '', 0))
    else:
        project.add(hours, minutes)

def save():
    os.system('mv %s %s' % (CONFIG_LOCATION, CONFIG_LOCATION+'.bak'))
    f = open(CONFIG_LOCATION, 'w')

    f.write('%% tags\n')

    for tag in tags:
        f.write(tag.save_str())

    f.write('%% data\n')

    for project in projects:
        f.write(project.save_str())

    f.close()

def get_args(argv):
    project_name = ''
    hours = 0
    minutes = 0

    for arg in argv[1:]:
        number = arg.rstrip()[:-1]

        if ('H' in arg or 'h' in arg) and (number.isnumeric() or 
                (number[0] == '-' and number[1:].isnumeric())):
            hours   = int(number)
        elif ('M' in arg or 'm' in arg) and (number.isnumeric() or
                (number[0] == '-' and number[1:].isnumeric())):
            minutes = int(number)
        else:
            project_name += arg + ' '

    return (project_name.rstrip(), hours, minutes)

def main():
    if len(sys.argv) > 1 and sys.argv[1] == '-h':
        print(USAGE)
        return

    load_file()

    args = get_args(sys.argv)

    # show all projects
    if len(args[0]) == 0:
        show_projects()

    # show specific project or tag
    elif args[1] == 0 and args[2] == 0:
        show_project(args[0])

    else:
        add_hours(args[0], args[1], args[2])
        show_project(args[0])

    save()

if __name__ == '__main__':
    main()
