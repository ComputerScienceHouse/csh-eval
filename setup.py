from setuptools import setup

with open('requirements.txt') as r:
    requirements = r.readlines()

with open('VERSION') as v:
    version = v.read()

setup(name='csh-eval',
      version=version,
      author='Matt Gambogi',
      author_email='gambogi@csh.rit.edu',
      url=('https://github.com/gambogi/csh-eval')
      packages=['csh.eval'],
      description='An evaluations system for the Computer Science House'
                  ' at RIT.',
      install_requires=requirements
)
